module type S = sig
  module IO : Mehari.Private.IO

  type handler = Eio.Net.Ipaddr.v4v6 Mehari.Private.Handler.Make(IO).t

  val run :
    ?port:int ->
    ?backlog:int ->
    ?timeout:float * Eio.Time.clock ->
    ?addr:Eio.Net.Ipaddr.v4v6 ->
    ?config:Tls.Config.server ->
    certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
    Eio.Net.t ->
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
    timeout : (float * Eio.Time.clock) option;
    tls_config : Tls.Config.server;
  }

  let make_config ~addr ~port ~timeout ~tls_config =
    { addr; port; timeout; tls_config }

  let src = Logs.Src.create "mehari.eio"

  module Log = (val Logs.src_log src)

  let write_resp flow resp =
    Buf_write.with_flow flow @@ fun w ->
    (match Mehari.Private.view_of_resp resp with
    | Immediate bufs -> List.iter (fun buf -> Buf_write.string w buf) bufs
    | Delayed d -> d (Buf_write.string w));
    Buf_write.flush w

  let client_req =
    let crlf = Buf_read.string "\r\n" in
    Buf_read.(Syntax.(take_while (fun c -> not (Char.equal c '\r')) <* crlf))

  let handle_client config callback flow ep =
    let reader =
      Buf_read.of_flow flow ~initial_size:1025
        ~max_size:1025 (* Apparently not inclusive *)
    in
    (try
       let sni, certificates =
         match ep with
         | Ok data ->
             ( Option.map Domain_name.to_string data.Tls.Core.own_name,
               data.Tls.Core.own_certificate )
         | Error () -> assert false
       in
       let hostnames =
         List.map Tls.Core.Cert.hostnames certificates
         |> List.fold_left X509.Host.Set.union X509.Host.Set.empty
         |> X509.Host.Set.to_seq
         |> Seq.map (fun (_, d) -> Domain_name.to_string d)
       in
       let timeout =
         match config.timeout with
         | None -> fun f -> f ()
         | Some (duration, clock) -> Eio.Time.with_timeout_exn clock duration
       in
       match
         timeout (fun () ->
             client_req reader
             |> Protocol.static_check_request ~port:config.port ~hostnames)
       with
       | Ok uri ->
           let client_cert =
             match ep with
             | Ok data -> Option.to_list data.Tls.Core.peer_certificate
             | Error () -> assert false
           in
           Mehari.Private.make_request
             (module Common.Addr)
             ~uri ~addr:config.addr ~port:config.port ~sni ~client_cert
           |> callback |> write_resp flow
       | Error err -> Protocol.to_response err |> write_resp flow
     with
    | Buf_read.Buffer_limit_exceeded ->
        Protocol.to_response AboveMaxSize |> write_resp flow
    | End_of_file -> Log.warn (fun log -> log "EOF encountered prematurly")
    | Failure _ -> Protocol.to_response InvalidURL |> write_resp flow
    | Eio.Time.Timeout ->
        Log.warn (fun log -> log "Timeout while reading client request"));
    flow#shutdown `Send

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
    (*| Eio.Exn.Io (Eio.Net.E (Connection_reset _), _) ->
        Log.warn (fun log -> log "Concurrent connections")
      FIXME: Removed due to unavailability outside of Linux *)
    | exn -> raise exn

  module Cert = Mehari.Private.Cert.Make (struct
    module IO = Common.Direct

    type path = Eio.Fs.dir Eio.Path.t

    include X509_eio
  end)

  let run ?(port = 1965) ?(backlog = 4096) ?timeout
      ?(addr = Net.Ipaddr.V4.loopback) ?config ~certchains net callback =
    let certificates = Cert.get_certs certchains ~exn_msg:"Mehari_eio.run" in
    let tls_config =
      match config with
      | Some c -> c
      | None ->
          Tls.Config.server ~certificates
            ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
            ()
    in
    let config = make_config ~addr ~port ~timeout ~tls_config in
    Eio.Switch.run (fun sw ->
        let socket =
          Net.listen ~reuse_addr:true ~reuse_port:true ~backlog ~sw net
            (`Tcp (addr, port))
        in
        let rec serve () =
          handler ~config callback
          |> Net.accept_fork ~sw ~on_error:log_err socket;
          serve ()
        in
        serve () |> ignore)
end
