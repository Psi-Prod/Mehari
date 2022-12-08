module type S = sig
  type stack

  val run :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    Handler.t ->
    unit Lwt.t
end

module Make (Stack : Tcpip.Stack.V4V6) : S with type stack := Stack.t = struct
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

  let write flow buf =
    match%lwt Cstruct.of_string buf |> TLS.write flow with
    | Ok () -> Lwt.return_unit
    | Error _ -> failwith "writing"

  let write_resp flow = function
    | Response.Immediate buf -> write flow buf
    | Stream stream -> Lwt_stream.iter_s (write flow) stream

  let read flow =
    match%lwt TLS.read flow with
    | Ok (`Data buffer) -> Cstruct.to_string buffer |> Lwt.return
    | Ok `Eof -> failwith "eof"
    | Error _ -> failwith "reading"

  let serve conf handler flow =
    let* server =
      match%lwt TLS.server_of_flow conf flow with
      | Ok s -> Lwt.return s
      | Error _ -> failwith "i hate god"
    in
    let* () = TLS.epoch server |> handler server (Stack.TCP.dst flow) in
    TLS.close server

  let start_server ~port ~certchains ~stack callback =
    let* certs = load_certs certchains in
    let handle_request flow addr ep =
      let* buf = read flow in
      let uri = String.(sub buf 0 (length buf - 2)) |> Uri.of_string in
      (* TODO: fix malformerd header *)
      let sni =
        match ep with
        | Ok data -> Option.map Domain_name.to_string data.Tls.Core.own_name
        | Error () -> assert false
      in
      let* resp = callback (Request.make ~addr ~uri ~sni) in
      write_resp flow resp
    in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "start_server"
    in
    serve (Tls.Config.server ~certificates ()) handle_request
    |> Stack.TCP.listen (Stack.tcp stack) ~port;
    Stack.listen stack

  let run ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ]) stack
      callback =
    start_server ~port ~certchains ~stack callback
end
