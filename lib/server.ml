open Lwt.Syntax

let load_certs certs =
  let rec aux acc = function
    | [] -> Lwt.return acc
    | (cert, priv_key) :: tl ->
        let* certchain = X509_lwt.private_of_pems ~cert ~priv_key in
        aux (certchain :: acc) tl
  in
  aux [] certs

let init_socket addr port =
  let sockaddr = Unix.ADDR_INET (addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind socket sockaddr in
  Lwt.return socket

let create_srv_socket addr port =
  let* socket = init_socket addr port in
  Lwt_unix.listen socket 10;
  Lwt.return socket

let write oc buff =
  let* () = Lwt_io.write oc buff in
  Lwt_io.flush oc

let read ic = Lwt_io.read ic ~count:2048

let rec serve handler sock certificates =
  let* sock_cl, addr = Lwt_unix.accept sock in
  let* server =
    Tls_lwt.Unix.server_of_fd (Tls.Config.server ~certificates ()) sock_cl
  in
  let ic, oc = Tls_lwt.of_t server in
  let* () = handler ic oc addr in
  let* () = Tls_lwt.Unix.close_tls server in
  serve handler sock certificates

let start_server ~address ~port ~certchains callback =
  let* certs = load_certs certchains in
  let handle_request ic oc addr =
    let* buf = read ic in
    let uri = String.(sub buf 0 (length buf - 2)) |> Uri.of_string in
    (* TODO: fix malformerd header *)
    let* resp = callback (Request.make ~addr ~uri) in
    write oc resp
  in
  let* sock =
    create_srv_socket
      (Option.value ~default:Unix.inet_addr_loopback address)
      port
  in
  serve handle_request sock @@ `Multiple_default (List.hd certs, certs)

let run_lwt ?(port = 1965) ?addr ?(certchains = [ ("./cert.pem", "./key.pem") ])
    callback =
  start_server ~port
    ~address:(Option.map Unix.inet_addr_of_string addr)
    ~certchains callback
  |> Lwt_main.run

let run ?(port = 1965) ?addr ?(certchains = [ ("./cert.pem", "./key.pem") ])
    callback =
  run_lwt ~port ?addr ~certchains callback
