open Mehari
module Protocol = Private.Protocol

let src = Logs.Src.create "mehari.miou-unx"

module Log = (val Logs.src_log src)

external reraise : exn -> 'a = "%reraise"

let log_err = function
  | `ClientReqAboveMaxSize ->
      Log.warn (fun log -> log "Client request is above max size")
  | `ConnectionClosed ->
      Log.warn (fun log -> log "Connection have been closed prematurely")
  | `ConnectionClosedWhileWriting ->
      Log.warn (fun log ->
          log
            "Connection have been closed prematurely by peer while writing \
             response")
  | `NotAbleToCompleteHandshake ->
      Log.warn (fun log -> log "Not able to complete the handshake")
  | `PrematureEofWhileReadingClientReq ->
      Log.warn (fun log ->
          log "Premature end of file while reading client request")
  | `Timeout -> Log.warn (fun log -> log "Timeout while reading client request")
  | `TLSAlert a ->
      Log.warn (fun log ->
          log "TLS alert: %S" @@ Tls.Packet.alert_type_to_string a)
  | `TLSFailure f ->
      Log.warn (fun log ->
          log "Tls failure: %S" @@ Tls.Engine.string_of_failure f)
  | `TLSReadError err ->
      Log.warn (fun log ->
          log "Error while reading TLS socket: %S" (Unix.error_message err))
  | `TLSWriteError err ->
      Log.warn (fun log ->
          log "Error while writing TLS socket: %s" (Unix.error_message err))

let write_response socket resp =
  let write buf = Tls_miou_unix.write socket buf in
  try
    let () =
      match Response.Private.view_of_resp resp with
      | Immediate buf -> write buf
      | Chunks { body; _ } -> body write
    in
    Ok (Tls_miou_unix.close socket)
  with
  | Unix.Unix_error (err, _, _) -> Error (`TLSWriteError err)
  | Tls_miou_unix.Closed_by_peer -> Error `ConnectionClosedWhileWriting

let read_client_request ?timeout fd =
  let parse_request fd =
    let buf = Buffer.create 1024 in
    let rec loop n cr =
      let read_char fd =
        let buf = Bytes.create 1 in
        if Tls_miou_unix.read fd buf = 0 then `Eof
        else `Data (Bytes.unsafe_get buf 0)
      in
      match read_char fd with
      | `Data _ when n > 1024 -> Error `ClientReqAboveMaxSize
      | `Data '\n' when cr -> Ok (Buffer.contents buf)
      | `Data '\r' ->
          let n = if cr then n + 1 else n in
          loop n true
      | `Data c ->
          Buffer.add_char buf c;
          let n = if cr then n + 1 else n in
          loop (n + 1) false
      | `Eof -> Error `PrematureEofWhileReadingClientReq
      | exception Unix.Unix_error (err, _, _) -> Error (`TLSReadError err)
    in
    loop 0 false
  in
  let with_timeout timeout f =
    let exception Timeout in
    match timeout with
    | None -> f ()
    | Some duration -> (
        let timeout =
          Miou.async (fun () ->
              Miou_unix.sleep duration;
              raise Timeout)
        in
        match Miou.await_first [ timeout; Miou.async f ] with
        | Ok r -> r
        | Error Timeout -> Error `Timeout
        | Error exn -> reraise exn)
  in
  with_timeout timeout (fun () ->
      parse_request fd
      |> Result.map (fun req ->
             let occured_time =
               Unix.gettimeofday () |> Ptime.of_float_s |> Option.get
             in
             (req, occured_time)))

let handle_client ~client_ip ~port ~timeout tls_config socket handler =
  try
    let socket = Tls_miou_unix.server_of_fd tls_config socket in
    match read_client_request ?timeout socket with
    | Ok (client_request, timestamp) -> (
        match Tls_miou_unix.epoch socket with
        | Some { Tls.Core.own_name; peer_certificate; protocol_version; _ } ->
            let request =
              Protocol.make_request ~client_ip ?hostname:own_name ~port
                ~tls_version:protocol_version ?client_cert:peer_certificate
                ~client_request ~now:timestamp ()
            in
            let response =
              match request with
              | Ok req -> handler req
              | Error err -> Protocol.to_response err
            in
            write_response socket response
        | None -> Error `ConnectionClosed)
    | Error `ClientReqAboveMaxSize ->
        write_response socket @@ Protocol.to_response AboveMaxSize
    | Error _ as err -> err
  with
  | End_of_file -> Error `NotAbleToCompleteHandshake
  | Tls_miou_unix.Tls_alert a -> Error (`TLSAlert a)
  | Tls_miou_unix.Tls_failure f -> Error (`TLSFailure f)

let clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> (
      match Miou.await prm with Ok () -> () | Error exn -> raise exn)

let ipaddr_of_sockaddr = function
  | Unix.ADDR_UNIX _ -> assert false
  | ADDR_INET (inet_addr, _) ->
      Unix.string_of_inet_addr inet_addr |> Ipaddr.of_string_exn

let run ?(port = 1965) ?timeout ?(config = Config.make ()) ~certs handler =
  let { Config.ip; backlog; reuseaddr; reuseport } = config in
  let socket, inet_addr =
    match ip with
    | V4 ip -> (Miou_unix.tcpv4 (), Ipaddr.V4.Prefix.to_string ip)
    | V6 ip -> (Miou_unix.tcpv6 (), Ipaddr.V6.Prefix.to_string ip)
  in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string inet_addr, port) in
  Miou_unix.bind_and_listen ~backlog ~reuseaddr ~reuseport socket sockaddr;
  let orphans = Miou.orphans () in
  while true do
    clean_up orphans;
    let client, client_ip = Miou_unix.accept socket in
    ignore
      (Miou.async ~orphans (fun () ->
           let tls_config = Certs.Private.make_config certs in
           handle_client
             ~client_ip:(ipaddr_of_sockaddr client_ip)
             ~port ~timeout tls_config client handler
           |> Result.iter_error log_err))
  done;
  Miou_unix.close socket
