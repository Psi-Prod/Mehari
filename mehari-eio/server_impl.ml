open Mehari

module type S = sig
  module IO : Private.IO

  type handler = request -> response IO.t

  val run :
    ?port:int ->
    ?verify_url_host:bool ->
    ?timeout:float ->
    ?config:Config.t ->
    certs:Certs.t ->
    handler ->
    unit IO.t
end

type config = {
  env : Identity_reader_monad.env;
  port : int;
  timeout : float option;
  certs : Certs.t;
  verify_url_host : bool;
}

let make_config ~env ~port ~timeout ~certs ~verify_url_host =
  { env; port; timeout; certs; verify_url_host }

module Make (Logger : Private.Signatures.LOGGER) :
  S with module IO = Identity_reader_monad = struct
  module IO = Identity_reader_monad

  type handler = request -> response IO.t

  module Buf_read = Eio.Buf_read
  module Buf_write = Eio.Buf_write
  module Net = Eio.Net
  module Protocol = Private.Protocol

  let src = Logs.Src.create "mehari.eio"

  module Log = (val Logs.src_log src)

  let read_client_request ?timeout clock flow =
    let client_req =
      let crlf = Buf_read.string "\r\n" in
      Buf_read.(Syntax.(take_while (fun c -> not (Char.equal c '\r')) <* crlf))
    in
    let reader =
      Buf_read.of_flow flow ~initial_size:1025
        ~max_size:1025 (* Apparently not inclusive *)
    in
    let with_timeout clock timeout f =
      match timeout with
      | None -> f ()
      | Some duration -> Eio.Time.with_timeout_exn clock duration f
    in
    with_timeout clock timeout (fun () -> client_req reader)

  let write_response flow resp =
    let () =
      Buf_write.with_flow flow @@ fun w ->
      match Response.Private.view_of_resp resp with
      | Immediate buf ->
          Buf_write.string w buf;
          Buf_write.flush w
      | Chunks { body; flush } ->
          let consume buf =
            if flush then (
              Buf_write.string w buf;
              Buf_write.flush w)
            else Buf_write.string w buf
          in
          body consume
    in
    Eio.Flow.shutdown flow `Send

  let gemini_exchange ~client_ip
      { certs; port; env; timeout; verify_url_host; _ } flow handler =
    let client_request = read_client_request ?timeout env#clock flow in
    let { Tls.Core.own_name; peer_certificate; protocol_version; _ } =
      match Tls_eio.epoch flow with
      | Ok data -> data
      | Error () -> raise End_of_file
    in
    let response =
      let request =
        Protocol.make_request ~client_ip ?hostname:own_name ~port
          ~verify_url_host ~tls_version:protocol_version
          ?client_cert:peer_certificate ~client_request certs
      in
      match request with
      | Ok req -> handler req
      | Error err -> Protocol.to_response err
    in
    write_response flow response

  let handle_client ~client_ip config flow handler =
    try gemini_exchange ~client_ip config flow handler with
    | Buf_read.Buffer_limit_exceeded ->
        Protocol.to_response AboveMaxSize |> write_response flow
    | Failure _ -> Protocol.to_response InvalidURL |> write_response flow
    | End_of_file -> Log.warn (fun log -> log "EOF encountered prematurly")
    | Eio.Time.Timeout ->
        Log.warn (fun log -> log "Timeout while reading client request")

  let log_err = function
    | End_of_file -> Log.warn (fun log -> log "Client closed socket prematurly")
    | Tls_eio.Tls_alert a ->
        Log.warn (fun log ->
            log "Tls alert: %S" @@ Tls.Packet.alert_type_to_string a)
    | Tls_eio.Tls_failure f ->
        Log.warn (fun log ->
            log "Tls failure: %S" @@ Tls.Engine.string_of_failure f)
    | Eio.Exn.Io (Eio.Net.E (Connection_reset _), _) ->
        Log.warn (fun log -> log "Concurrent connections")
    | exn -> raise exn

  let run ?(port = 1965) ?(verify_url_host = true) ?timeout
      ?(config = Config.make ()) ~certs handler env =
    Eio.Switch.run (fun sw ->
        let socket =
          Net.listen ~reuse_addr:true ~reuse_port:true ~backlog:config.backlog ~sw env#net
            (`Tcp (config.addr, port))
        in
        Log.info (fun log -> log "Listening on port %i" port);
        Eio.Net.run_server ~on_error:log_err socket (fun flow -> function
          | `Tcp (client_ip, _) ->
              let client_ip =
                Ipaddr.of_octets_exn (client_ip : Net.Ipaddr.v4v6 :> string)
              in
              let tls_config = Certs.Private.make_config certs in
              let srv = Tls_eio.server_of_flow tls_config flow in
              let config =
                make_config ~env ~port ~timeout ~certs ~verify_url_host
              in
              handle_client ~client_ip config srv (fun req -> handler req env)
          | `Unix _ -> assert false (* We listen on a TCP socket. *)))
end
