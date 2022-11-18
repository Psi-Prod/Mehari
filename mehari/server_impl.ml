module Make
    (Clock : Mirage_clock.PCLOCK)
    (KV : Mirage_kv.RO)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module TLS = Tls_mirage.Make (Stack.TCP)
  module X509 = Tls_mirage.X509 (KV) (Clock)
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
      write flow resp
    in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "start_server"
    in
    Lwt.return
    @@ Stack.TCP.listen stack ~port
         (serve (Tls.Config.server ~certificates ()) handle_request)

  let run_lwt ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ])
      stack callback =
    start_server ~port ~certchains ~stack callback |> Lwt_main.run

  let run ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ]) stack
      callback =
    run_lwt ~port ~certchains stack callback
end
