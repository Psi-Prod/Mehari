module type S = sig
  module IO : Mehari.Private.IO

  type handler = Eio.Net.Ipaddr.v4v6 Mehari.Private.Handler.Make(IO).t

  val run :
    ?port:int ->
    ?verify_url_host:bool ->
    ?config:Tls.Config.server ->
    ?timeout:float * [ `Clock of float ] Eio.Time.clock ->
    ?backlog:int ->
    ?addr:Eio.Net.Ipaddr.v4v6 ->
    certchains:Tls.Config.certchain list ->
    _ Eio.Net.t ->
    handler ->
    unit
end

module Make (Logger : Mehari.Private.Logger_impl.S) :
  S with module IO = Common.Direct = struct
  module IO = Common.Direct

  type handler = Eio.Net.Ipaddr.v4v6 Mehari.Private.Handler.Make(IO).t

  module Buf_read = Eio.Buf_read
  module Buf_write = Eio.Buf_write
  module Net = Eio.Net
  module Protocol = Mehari.Private.Protocol

  type config = {
    addr : Net.Ipaddr.v4v6;
    port : int;
    timeout : (float * [ `Clock of float ] Eio.Time.clock) option;
    tls_config : Tls.Config.server;
    certs : X509.Certificate.t list;
    verify_url_host : bool;
  }

  let make_config ~addr ~port ~timeout ~tls_config ~certs ~verify_url_host =
    { addr; port; timeout; tls_config; certs; verify_url_host }

  let src = Logs.Src.create "mehari.eio"

  module Log = (val Logs.src_log src)

  let write_resp flow resp =
    Buf_write.with_flow flow @@ fun w ->
    match Mehari.Private.view_of_resp resp with
    | Immediate bufs ->
        List.iter (fun buf -> Buf_write.string w buf) bufs;
        Buf_write.flush w
    | Delayed { body; flush } ->
        let consume buf =
          if flush then (
            Buf_write.string w buf;
            Buf_write.flush w)
          else Buf_write.string w buf
        in
        body consume

  let client_req =
    let crlf = Buf_read.string "\r\n" in
    Buf_read.(Syntax.(take_while (fun c -> not (Char.equal c '\r')) <* crlf))

  let handle_client config callback flow epoch =
    let reader =
      Buf_read.of_flow flow ~initial_size:1025
        ~max_size:1025 (* Apparently not inclusive *)
    in
    (try
       let ep =
         match epoch with Ok data -> data | Error () -> raise End_of_file
       in
       let with_timeout =
         match config.timeout with
         | None -> fun f -> f ()
         | Some (duration, clock) -> Eio.Time.with_timeout_exn clock duration
       in
       match
         with_timeout (fun () -> client_req reader)
         |> Protocol.make_request
              (module Common.Addr)
              ~port:config.port ~addr:config.addr
              ~verify_url_host:config.verify_url_host config.certs ep
       with
       | Ok req -> callback req |> write_resp flow
       | Error err -> Protocol.to_response err |> write_resp flow
     with
    | Buf_read.Buffer_limit_exceeded ->
        Protocol.to_response AboveMaxSize |> write_resp flow
    | End_of_file -> Log.warn (fun log -> log "EOF encountered prematurly")
    | Failure _ -> Protocol.to_response InvalidURL |> write_resp flow
    | Eio.Time.Timeout ->
        Log.warn (fun log -> log "Timeout while reading client request"));
    Eio.Flow.shutdown flow `Send

  let handler ~config callback flow _ =
    let server = Tls_eio.server_of_flow config.tls_config flow in
    Tls_eio.epoch server |> handle_client config callback server

  let log_err = function
    | End_of_file -> Log.warn (fun log -> log "Client closed socket prematurly")
    | Tls_eio.Tls_alert a ->
        Log.warn (fun log ->
            log "Tls alert: %S" (Tls.Packet.alert_type_to_string a))
    | Tls_eio.Tls_failure f ->
        Log.warn (fun log ->
            log "Tls failure: %S" (Tls.Engine.string_of_failure f))
    | Eio.Exn.Io (Eio.Net.E (Connection_reset _), _) ->
        Log.warn (fun log -> log "Concurrent connections")
    | exn -> raise exn

  let run ?(port = 1965) ?(verify_url_host = true) ?config ?timeout
      ?(backlog = 4096) ?(addr = Net.Ipaddr.V4.loopback) ~certchains net
      callback =
    let certificates =
      Mehari.Private.Cert.get_certs certchains ~exn_msg:"Mehari_eio.run"
    in
    let tls_config =
      match config with
      | Some c -> c
      | None ->
          Tls.Config.server ~certificates
            ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
            ()
    in
    let config =
      make_config ~addr ~port ~timeout ~tls_config
        ~certs:(List.concat_map fst certchains)
        ~verify_url_host
    in
    Eio.Switch.run (fun sw ->
        let socket =
          Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw net
            (`Tcp (addr, port))
        in
        Log.info (fun log -> log "Listening on port %i" port);
        handler ~config callback |> Eio.Net.run_server ~on_error:log_err socket)
end
