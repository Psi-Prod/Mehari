module type S = sig
  type stack

  val run :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    Handler.t ->
    unit Lwt.t
end

module Make
    (Clock : Mirage_clock.PCLOCK)
    (KV : Mirage_kv.RO)
    (Stack : Tcpip.Stack.V4V6) : S with type stack := Stack.TCP.t = struct
  module TLS = Tls_mirage.Make (Stack.TCP)
  open Lwt.Syntax

  let load_certs certs =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (cert, priv_key) :: tl ->
          let* certchain = X509_lwt.private_of_pems ~cert ~priv_key in
          aux (certchain :: acc) tl
    in
    aux [] certs

  let write flow buff =
    let* result = TLS.write flow buff in
    match result with Ok () -> Lwt.return_unit | Error _ -> failwith "writing"

  let read flow =
    let* result = TLS.read flow in
    match result with
    | Ok (`Data buffer) -> Lwt.return @@ Cstruct.to_string buffer
    | Ok `Eof -> failwith "eof"
    | Error _ -> failwith "reading"

  let serve conf handler flow =
    let* server_r = TLS.server_of_flow conf flow in
    let server =
      match server_r with Ok s -> s | Error _ -> failwith "i hate god"
    in
    let* () = TLS.epoch server |> handler server (Stack.TCP.dst flow) in
    TLS.close server

  let start_server ~port ~certchains ~stack callback =
    let* certs = load_certs certchains in
    let handle_request flow addr ep =
      let* buf = read flow in
      let uri = String.(sub buf 0 (length buf - 2)) |> Uri.of_string in
      (* TODO: fix malformerd header *)
      let* resp =
        callback
          (Request.make ~addr ~uri
             ~sni:
               (match ep with
               | Ok data ->
                   Option.map Domain_name.to_string data.Tls.Core.own_name
               | Error () -> assert false))
      in
      Cstruct.of_string resp |> write flow
    in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "start_server"
    in
    Stack.TCP.listen stack ~port
      (serve (Tls.Config.server ~certificates ()) handle_request)
    |> Lwt.return

  let run ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ]) stack
      callback =
    start_server ~port ~certchains ~stack callback
end

module TempServer : S with type stack := string = struct
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
    let* () = Tls_lwt.Unix.epoch server |> handler ic oc addr in
    let* () = Tls_lwt.Unix.close_tls server in
    serve handler sock certificates

  let start_server ~address ~port ~certchains callback =
    let* certs = load_certs certchains in
    let handle_request ic oc addr ep =
      let* buf = read ic in
      let uri = String.(sub buf 0 (length buf - 2)) |> Uri.of_string in
      (* TODO: fix malformerd header *)
      let* resp =
        callback
          (Request.make
             ~addr:
               (match addr with
               | Unix.ADDR_INET (a, i) ->
                   (Unix.string_of_inet_addr a |> Ipaddr.of_string_exn, i)
               | _ -> assert false)
             ~uri
             ~sni:
               (match ep with
               | Ok data ->
                   Option.map Domain_name.to_string data.Tls.Core.own_name
               | Error () -> assert false))
      in
      write oc resp
    in
    let* sock =
      create_srv_socket
        (Option.value ~default:Unix.inet_addr_loopback address)
        port
    in
    match certs with
    | c :: _ -> `Multiple_default (c, certs) |> serve handle_request sock
    | _ -> invalid_arg "start_server"

  let run ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ]) stack
      callback =
    start_server ~port
      ~address:(Some (Unix.inet_addr_of_string stack))
      ~certchains callback
end
