open Mehari

module type S = sig
  module IO : Private.IO

  type handler = request -> response IO.t

  val run :
    ?port:int ->
    ?verify_url_host:bool ->
    ?timeout:float ->
    ?backlog:int ->
    ?addr:Eio.Net.Ipaddr.v4v6 ->
    certchains:Tls.Config.certchain list ->
    handler ->
    unit IO.t
end

module Make (Logger : Private.Logger_impl.S) :
  S with module IO = Identity_reader_monad = struct
  module IO = Identity_reader_monad

  type handler = request -> response IO.t

  module Buf_read = Eio.Buf_read
  module Buf_write = Eio.Buf_write
  module Net = Eio.Net
  module Protocol = Private.Protocol

  type config = {
    env : Identity_reader_monad.env;
    addr : Ipaddr.t;
    port : int;
    timeout : float option;
    tls_config : Tls.Config.server;
    certs : X509.Certificate.t list;
    verify_url_host : bool;
  }

  let make_config ~env ~(addr : Net.Ipaddr.v4v6) ~port ~timeout ~tls_config
      ~certs ~verify_url_host =
    {
      env;
      addr = Ipaddr.of_octets_exn (addr :> string);
      port;
      timeout;
      tls_config;
      certs;
      verify_url_host;
    }

  let src = Logs.Src.create "mehari.eio"

  module Log = (val Logs.src_log src)

  let write_resp flow resp =
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

  let client_req =
    let crlf = Buf_read.string "\r\n" in
    Buf_read.(Syntax.(take_while (fun c -> not (Char.equal c '\r')) <* crlf))

  let handle_client config handler flow epoch =
    let reader =
      Buf_read.of_flow flow ~initial_size:1025
        ~max_size:1025 (* Apparently not inclusive *)
    in
    (try
       let { Tls.Core.own_name; peer_certificate; protocol_version; _ } =
         match epoch with Ok data -> data | Error () -> raise End_of_file
       in
       let with_timeout =
         match config.timeout with
         | None -> fun f -> f ()
         | Some duration -> Eio.Time.with_timeout_exn config.env#clock duration
       in
       match
         let client_request = with_timeout (fun () -> client_req reader) in
         let tls_version =
           (* We explicitely don't support TLS version < 1.2. *)
           match protocol_version with
           | `TLS_1_0 | `TLS_1_1 -> assert false
           | (`TLS_1_2 | `TLS_1_3) as version -> version
         in
         Protocol.make_request ~port:config.port
           ~client_addr:config.addr (* TODO: pass REAL client address *)
           ?hostname:own_name ~verify_url_host:config.verify_url_host
           ~tls_version ?client_cert:peer_certificate ~client_request
           config.certs
       with
       | Ok req -> handler req |> write_resp flow
       | Error err -> Protocol.to_response err |> write_resp flow
     with
    | Buf_read.Buffer_limit_exceeded ->
        Protocol.to_response AboveMaxSize |> write_resp flow
    | End_of_file -> Log.warn (fun log -> log "EOF encountered prematurly")
    | Failure _ -> Protocol.to_response InvalidURL |> write_resp flow
    | Eio.Time.Timeout ->
        Log.warn (fun log -> log "Timeout while reading client request"));
    Eio.Flow.shutdown flow `Send

  let callback ~config handler flow _ =
    let server = Tls_eio.server_of_flow config.tls_config flow in
    Tls_eio.epoch server |> handle_client config handler server

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

  let run ?(port = 1965) ?(verify_url_host = true) ?timeout ?(backlog = 4096)
      ?(addr = Net.Ipaddr.V4.loopback) ~certchains handler env =
    let certificates =
      Private.Cert.get_certs certchains ~exn_msg:"Mehari_eio.run"
    in
    let tls_config =
      Tls.Config.server ~version:(`TLS_1_2, `TLS_1_3) ~certificates
        ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
        ()
      |> Result.get_ok
    in
    let config =
      make_config ~env ~addr ~port ~timeout ~tls_config
        ~certs:(List.concat_map fst certchains)
        ~verify_url_host
    in
    Eio.Switch.run (fun sw ->
        let socket =
          Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw env#net
            (`Tcp (addr, port))
        in
        Log.info (fun log -> log "Listening on port %i" port);
        let handler req = handler req env in
        callback ~config handler |> Eio.Net.run_server ~on_error:log_err socket)
end
