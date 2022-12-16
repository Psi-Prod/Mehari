open Mehari

module type S = sig
  type stack

  module IO : Private.IO

  type handler = Private.Handler.Make(IO).t

  val run :
    ?backlog:int ->
    ?address:Eio.Net.Sockaddr.stream ->
    ?port:int ->
    stack ->
    handler ->
    unit
end

module Make (Logger : Private.Logger_impl.S) = struct
  module IO = Direct

  type handler = Private.Handler.Make(IO).t

  module Read = Eio.Buf_read
  module Write = Eio.Buf_write

  let load_certs certs =
    let rec aux acc = function
      | [] -> acc
      | (cert, priv_key) :: tl ->
          aux (X509_eio.private_of_pems ~cert ~priv_key :: acc) tl
    in
    aux [] certs

  let write_resp flow resp =
    match Mehari.Private.view_of_resp resp with
    | Immediate bufs ->
        Write.with_flow flow (fun w ->
            List.iter (fun buf -> Write.string w buf) bufs;
            Write.flush w)
    | _ -> failwith "todo"

  let client_req = Re.(compile (seq [ group (rep1 any); char '\r'; char '\n' ]))

  let handle_client callback (flow : Eio.Flow.two_way) ep =
    let req = Read.parse_exn ~initial_size:1024 ~max_size:1024 Read.line flow in
    Eio.traceln "CLIENT: %S" req;
    let resp =
      match Re.exec_opt client_req req with
      | None -> (response bad_request) ""
      | Some grp ->
          let uri = Re.Group.get grp 1 |> Uri.of_string in
          let _sni =
            match ep with
            | Ok data -> Option.map Domain_name.to_string data.Tls.Core.own_name
            | Error () -> assert false
          in
          uri |> callback
    in
    write_resp flow resp

  let serve conf handler flow =
    let server = Tls_eio.server_of_flow conf flow in
    Tls_eio.epoch server |> handler server flow

  let handler ~certchains callback flow _ =
    let certs = load_certs certchains in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "start_server"
    in
    let server =
      Tls_eio.server_of_flow (Tls.Config.server ~certificates ()) flow
    in
    Tls_eio.epoch server |> handle_client callback flow

  let run ?(port = 1965) ?(backlog = 10) ?(addr = Eio.Net.Ipaddr.V4.loopback)
      ~certchains net callback =
    Eio.Switch.run (fun sw ->
        let socket =
          Eio.Net.listen ~reuse_addr:true ~backlog ~sw net (`Tcp (addr, port))
        in
        let rec loop () =
          handler ~certchains callback
          |> Eio.Net.accept_fork ~sw ~on_error:raise socket;
          loop ()
        in
        loop ())
end
