module Private = Mehari.Private

module type S = sig
  type stack

  module IO : Private.IO

  type handler = Ipaddr.t Private.Handler.Make(IO).t

  val run_lwt :
    ?port:int ->
    ?timeout:float ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t

  val run :
    ?port:int ->
    ?timeout:float ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit
end

module Make
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S)
    (Logger : Private.Logger_impl.S) :
  S with module IO = Lwt and type stack := Stack.t = struct
  module IO = Lwt

  type handler = Ipaddr.t Private.Handler.Make(IO).t

  module TLS = Tls_mirage.Make (Stack.TCP)
  module Channel = Mirage_channel.Make (TLS)
  module Protocol = Mehari.Private.Protocol

  module Cert = Mehari.Private.Cert.Make (struct
    module IO = IO

    type path = string

    include X509_lwt
  end)

  open Lwt.Syntax

  type config = {
    addr : Ipaddr.t;
    port : int;
    timeout : float option;
    tls_config : Tls.Config.server;
  }

  let make_config ~addr ~port ~timeout ~tls_config =
    { addr; port; timeout; tls_config }

  let src = Logs.Src.create "mehari.mirage"

  module Log = (val Logs.src_log src)

  let write_resp chan resp =
    let write buf = Channel.write_string chan buf 0 (String.length buf) in
    (match Mehari.Private.view_of_resp resp with
    | Immediate bufs -> List.iter write bufs
    | Delayed d -> d write);
    match%lwt Channel.flush chan with
    | Ok () -> Lwt.return_ok ()
    | Error err -> `ChannelWriteErr err |> Lwt.return_error

  let read_client_req flow =
    let buf = Buffer.create 1024 in
    let rec loop n cr =
      match%lwt Channel.read_char flow with
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

  exception Timeout

  let with_timeout _timeout f =
    match Some 1.0 with
    | None -> f ()
    | Some duration ->
        let timeout =
          let* () = Time.sleep_ns (Duration.of_f duration) in
          Lwt.fail Timeout
        in
        Lwt.pick [ f (); timeout ]

  let write_and_close chan flow resp =
    match%lwt write_resp chan resp with
    | Ok () ->
        let+ () = TLS.close flow in
        Ok ()
    | Error err -> Lwt.return_error err

  let handle_client config callback flow ep =
    let chan = Channel.create flow in
    match%lwt with_timeout config.timeout (fun () -> read_client_req chan) with
    | Ok client_req ->
        let sni, certificates, client_cert =
          match ep with
          | Ok data ->
              ( Option.map Domain_name.to_string data.Tls.Core.own_name,
                data.Tls.Core.own_certificate,
                Option.to_list data.Tls.Core.peer_certificate )
          | Error () -> assert false
        in
        let hostnames =
          List.map Tls.Core.Cert.hostnames certificates
          |> List.fold_left X509.Host.Set.union X509.Host.Set.empty
          |> X509.Host.Set.to_seq
          |> Seq.map (fun (_, d) -> Domain_name.to_string d)
        in
        let* resp =
          match
            Protocol.static_check_request ~port:config.port ~hostnames
              client_req
          with
          | Ok uri ->
              Mehari.Private.make_request
                (module Ipaddr)
                ~uri ~addr:config.addr ~port:config.port ~sni ~client_cert
              |> callback
          | Error err -> Protocol.to_response err |> Lwt.return
        in
        write_and_close chan flow resp
    | Error `BufferLimitExceeded ->
        Protocol.to_response AboveMaxSize |> write_and_close chan flow
    | Error err -> Lwt.return_error err

  let handler config callback flow =
    match%lwt TLS.server_of_flow config.tls_config flow with
    | Ok server -> TLS.epoch server |> handle_client config callback server
    | Error err -> `TLSWriteErr err |> Lwt.return_error

  let log_err = function
    | `BufferLimitExceeded -> assert false
    | `Eof -> Log.warn (fun log -> log "EOF encountered prematurly")
    | `ChannelWriteErr err ->
        Log.warn (fun log ->
            log "ChannelWriteErr: %a" Channel.pp_write_error err)
    | `ChannelErr err -> Log.warn (fun log -> log "%a" Channel.pp_error err)
    | `Timeout ->
        Log.warn (fun log -> log "Timeout while reading client request")
    | `TLSWriteErr err ->
        Log.warn (fun log -> log "TLSWriteErr%a" TLS.pp_write_error err)

  let run_lwt ?(port = 1965) ?timeout ?config
      ?(certchains = [ ("./cert.pem", "./key.pem") ]) stack callback =
    let* certs = load_certs certchains in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "Mehari_mirage.run_lwt"
    in
    let addr =
      Stack.ip stack |> Stack.IP.get_ip
      |> Fun.flip List.nth 0 (* List should not be empty. *)
    in
    let tls_config =
      match config with
      | Some c -> c
      | None ->
          Tls.Config.server ~certificates
            ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
            ()
    in
    let config = make_config ~addr ~port ~timeout ~tls_config in
    Stack.TCP.listen (Stack.tcp stack) ~port (fun flow ->
        match%lwt handler config callback flow with
        | Ok () -> Lwt.return_unit
        | exception Timeout ->
            log_err `Timeout;
            Lwt.return_unit
        | Error err ->
            log_err err;
            Lwt.return_unit);
    Stack.listen stack

  let run ?port ?timeout ?config ?certchains stack callback =
    run_lwt ?port ?timeout ?config ?certchains stack callback |> Lwt_main.run
end
