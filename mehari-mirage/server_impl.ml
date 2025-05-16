open Mehari
open Lwt.Infix
open Lwt.Syntax

type config = {
  port : int;
  timeout : float option;
  certs : Certs.t;
  verify_url_host : bool;
}

let make_config ~port ~timeout ~certs ~verify_url_host =
  { port; timeout; certs; verify_url_host }

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S)
    (Logger : Private.Signatures.LOGGER) =
struct
  module IO = Lwt
  module TLS = Tls_mirage.Make (Stack.TCP)
  module Channel = Mirage_channel.Make (TLS)
  module Protocol = Private.Protocol

  type stack = Stack.t
  type handler = request -> response IO.t

  let src = Logs.Src.create "mehari.mirage"

  module Log = (val Logs.src_log src)

  let log_err = function
    | `BufferLimitExceeded -> assert false
    | `ConnectionClosed ->
        Log.warn (fun log -> log "Connection has been closed prematurly")
    | `Eof -> Log.warn (fun log -> log "EOF encountered prematurly")
    | `ChannelWriteErr err ->
        Log.warn (fun log ->
            log "ChannelWriteErr: %a" Channel.pp_write_error err)
    | `ChannelErr err -> Log.warn (fun log -> log "%a" Channel.pp_error err)
    | `Timeout ->
        Log.warn (fun log -> log "Timeout while reading client request")
    | `TLSWriteErr err ->
        Log.warn (fun log -> log "TLSWriteErr: %a" TLS.pp_write_error err)

  let write_response chan flow resp =
    let write buf = Channel.write_string chan buf 0 (String.length buf) in
    let flush chan =
      Channel.flush chan |> Lwt_result.map_error (fun e -> `ChannelWriteErr e)
    in
    let () =
      match Response.Private.view_of_resp resp with
      | Immediate bufs -> write bufs
      | Chunks { body; _ } -> body write
    in
    flush chan >>= function
    | Ok () -> TLS.close flow >|= Result.ok
    | Error err -> Lwt.return_error err

  let read_client_request ?timeout chan =
    let parse_request chan =
      let buf = Buffer.create 1024 in
      let rec loop n cr =
        Channel.read_char chan >>= function
        | Ok (`Data _) when n > 1024 -> Lwt.return_error `BufferLimitExceeded
        | Ok (`Data '\n') when cr -> Buffer.contents buf |> Lwt.return_ok
        | Ok (`Data '\r') -> loop n true
        | Ok (`Data c) ->
            Buffer.add_char buf c;
            loop (n + 1) false
        | Ok `Eof -> Lwt.return_error `Eof
        | Error err -> `ChannelErr err |> Lwt.return_error
      in
      loop 0 false
    in
    let with_timeout timeout f =
      let exception Timeout in
      match timeout with
      | None -> f ()
      | Some duration ->
          let timeout =
            let* () = Time.sleep_ns (Duration.of_f duration) in
            Lwt.fail Timeout
          in
          Lwt.catch
            (fun () -> Lwt.pick [ f (); timeout ])
            (function
              | Timeout ->
                  Lwt.return_error `Timeout
              | exn -> raise exn)
    in
    with_timeout timeout (fun () -> parse_request chan)

  let handle_client ~client_ip { certs; port; timeout; verify_url_host; _ } flow
      handler =
    let chan = Channel.create flow in
    read_client_request ?timeout chan >>= function
    | Ok client_request -> (
        match TLS.epoch flow with
        | Ok { Tls.Core.own_name; peer_certificate; protocol_version; _ } ->
            let request =
              Protocol.make_request ~client_ip ?hostname:own_name ~port
                ~verify_url_host ~tls_version:protocol_version
                ?client_cert:peer_certificate ~client_request certs
            in
            let* response =
              match request with
              | Ok req -> handler req
              | Error err -> Protocol.to_response err |> Lwt.return
            in
            write_response chan flow response
        | Error () -> Lwt.return_error `ConnectionClosed)
    | Error `BufferLimitExceeded ->
        Protocol.to_response AboveMaxSize |> write_response chan flow
    | Error err -> Lwt.return_error err

  let handler ~client_ip config tls_config callback flow =
    TLS.server_of_flow tls_config flow >>= function
    | Ok server -> handle_client ~client_ip config server callback
    | Error err -> Lwt.return_error (`TLSWriteErr err)

  let run ?(port = 1965) ?(verify_url_host = true) ?timeout ~certs stack
      callback =
    Logger.info (fun log -> log "Listening on port %i" port);
    Stack.TCP.listen (Stack.tcp stack) ~port (fun flow ->
        let client_ip, _ = Stack.TCP.dst flow in
        let config = make_config ~port ~timeout ~certs ~verify_url_host in
        let tls_config = Certs.Private.make_config certs in
        handler ~client_ip config tls_config callback flow
        >|= Result.iter_error log_err);
    Stack.listen stack
end
