open Lwt.Syntax

let cert =
  X509_lwt.private_of_pems
  ~cert:"./cert.pem"
  ~priv_key:"./key.pem"

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

let accept sock =
  let* (sock_cl, addr) = Lwt_unix.accept sock in
  let ic = Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.input sock_cl in
  let oc = Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.output sock_cl in
  Lwt.return ((ic,oc), addr, sock_cl)

let write oc buff =
  let* () = Lwt_io.write oc buff in
  Lwt_io.flush oc

let read ic =
  let* buff = Lwt_io.read ic ~count:2048 in
  Printf.printf "%s%!" buff;
  Lwt.return buff

let rec serve handler sock certchain =
  let* (_, _, sock_cl) = accept sock in
  let* server =
    Tls_lwt.Unix.server_of_fd
      (Tls.Config.server ~certificates:(`Single certchain) ()) sock_cl
  in
  let ic, oc = Tls_lwt.of_t server in
  let* () = handler ic oc in
  let* () = Tls_lwt.Unix.close_tls server in
  serve handler sock certchain

let start_server callback =
  let handle_request ic oc =
    let* buff = read ic in
    write oc @@ callback buff
  in
  let* sock = create_srv_socket Unix.inet_addr_loopback 1965 in
  let* certchain = cert in
  serve handle_request sock certchain

let run callback =
  Lwt_main.run @@ start_server callback
