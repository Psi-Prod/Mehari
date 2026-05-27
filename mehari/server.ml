module type S = sig
  type stack

  val run :
    ?timeout:float ->
    ?port:int ->
    certs:Tls.Config.certchain list ->
    stack ->
    Handler.t ->
    unit
end

module Runtime = Runtime

module Make (R : Runtime.S) = struct
  open R
  open Result.Syntax

  type error =
    [ `Handshake_failed
    | Runtime.tls_error
    | `End_of_file
    | `Timeout
    | `Connection_closed ]

  let log_error err =
    Logs.err ~src:Logger.src @@ fun log ->
    match err with
    | `Handshake_failed -> log "Not able to complete TLS handshake with client."
    | `Tls_alert alert ->
        log "A TLS alert has been received during client request reading: ."
        (* %s *)
        (*   (Tls.Packet.alert_type_to_string alert) *)
    | `Tls_failure fail ->
        log "A TLS protocol error has occured during client request reading: ."
        (* %a Tls.Engine.pp_failure fail *)
    | `End_of_file -> log "Premature EOF during client request reading."
    | `Timeout -> log "Timeout during client request reading."
    | `Connection_closed ->
        log "Connection was closed by client during response sending."

  let read_char =
    let buf = Bytes.create 1 in
    fun flow ->
      try
        let+ () = TLS.really_read flow buf in
        Bytes.unsafe_get buf 0
      with End_of_file -> Error `End_of_file

  let read_request ?timeout flow =
    let parse_request () =
      let buf = Buffer.create 1024 in
      let rec loop read cr =
        if read > 1024 then Error `Req_above_max_size
        else
          let* char = read_char flow in
          match char with
          | '\n' when cr -> Ok (Buffer.contents buf)
          | '\r' ->
              let read = if cr then read + 1 else read in
              loop read true
          | c ->
              Buffer.add_char buf c;
              let read = if cr then read + 1 else read in
              loop (read + 1) false
      in
      loop 0 false
    in
    let with_timeout timeout f =
      let exception Timeout in
      match timeout with
      | None -> f ()
      | Some duration -> begin
          let timeout =
            Miou.async (fun () ->
                sleep duration;
                raise Timeout)
          in
          match Miou.await_first [ timeout; Miou.async f ] with
          | Ok r -> r
          | Error Timeout -> Error `Timeout
          | Error exn -> Miou.reraise exn
        end
    in
    with_timeout timeout (fun () -> parse_request ())

  let respond flow resp =
    let+ () =
      match Response.Private.to_view resp with
      | String s -> TLS.write flow s
      | Stream s ->
          let sink =
            Flux.Sink.fold
              (fun acc chunk ->
                Result.bind acc (fun () -> TLS.write flow chunk))
              (Ok ())
          in
          Flux.Stream.into sink s
    in
    TLS.close flow

  let handler ~timeout ~port ~tls_config flow handler =
    let open Result.Syntax in
    let* flow =
      try Ok (TCP.tls_upgrade tls_config flow)
      with End_of_file -> Error `Handshake_failed
    in
    match read_request ~timeout flow with
    | Ok client_request -> begin
        let now = now () in
        match TLS.epoch flow with
        | None -> Error `Connection_closed
        | Some { Tls.Core.own_name; peer_certificate; protocol_version; _ } ->
            let client = TLS.peer flow in
            let response =
              Protocol.make_request ~client ?hostname:own_name ~port
                ~tls_version:protocol_version ?client_cert:peer_certificate
                ~client_request ~now ()
              |> Result.fold ~ok:handler ~error:Protocol.to_response
            in
            respond flow response
      end
    | Error `Req_above_max_size ->
        respond flow (Protocol.to_response Above_max_size)
    | Error #error as err -> err

  let rec clean_up orphans =
    match Miou.care orphans with
    | None | Some None -> ()
    | Some (Some prm) ->
        Miou.await_exn prm;
        clean_up orphans

  let run ?(timeout = 5.) ?(port = 1965) ~certs stack server =
    let tls_config =
      match
        Tls.Config.server ~version:(`TLS_1_2, `TLS_1_3)
          ~certificates:(`Multiple certs)
          ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
          ()
      with
      | Ok cfg -> cfg
      | Error (`Msg msg) -> invalid_arg msg
    in
    let listen = R.listen stack port in
    Logs.info ~src:Logger.src (fun log -> log "Listening on port %i" port);
    while true do
      let orphans = Miou.orphans () in
      clean_up orphans;
      let flow = TCP.accept stack listen in
      ignore
      @@ Miou.async ~orphans (fun () ->
          handler ~timeout ~port ~tls_config flow server
          |> Result.iter_error log_error)
    done
end
