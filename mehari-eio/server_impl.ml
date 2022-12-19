module type S = sig
  module IO : Mehari.Private.IO

  type handler = Eio.Net.Ipaddr.v4v6 Mehari.Private.Handler.Make(IO).t

  val run :
    ?port:int ->
    ?backlog:int ->
    ?addr:Eio.Net.Ipaddr.v4v6 ->
    certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
    Eio.Net.t ->
    handler ->
    'a
end

module Make (Logger : Mehari.Private.Logger_impl.S) :
  S with module IO = Common.Direct = struct
  module IO = Common.Direct

  type handler = Eio.Net.Ipaddr.v4v6 Mehari.Private.Handler.Make(IO).t

  let load_certs certs =
    let rec aux acc = function
      | [] -> acc
      | (cert, priv_key) :: tl ->
          aux (X509_eio.private_of_pems ~cert ~priv_key :: acc) tl
    in
    aux [] certs

  open Eio

  let write_resp flow resp =
    Buf_write.with_flow flow @@ fun w ->
    (match Mehari.Private.view_of_resp resp with
    | Immediate bufs -> List.iter (fun buf -> Buf_write.string w buf) bufs
    | Delayed d -> d (Buf_write.string w));
    Buf_write.flush w

  let client_req =
    let crlf = Buf_read.string "\r\n" in
    Buf_read.(Syntax.(take_while (fun c -> not (Char.equal c '\r')) <* crlf))

  let handle_client ~addr ~port callback flow ep =
    let reader = Buf_read.of_flow flow ~initial_size:1024 ~max_size:1024 in
    let resp =
      match client_req reader with
      | req ->
          let uri = Uri.of_string req in
          let sni =
            match ep with
            | Ok data -> Option.map Domain_name.to_string data.Tls.Core.own_name
            | Error () -> assert false
          in
          Mehari.Private.make_request (module Common.Addr) ~uri ~addr ~port ~sni
          |> callback
      | exception Failure _ -> failwith "todo"
      | exception End_of_file -> failwith "todo"
      | exception Buf_read.Buffer_limit_exceeded ->
          Mehari.(response bad_request) ""
    in
    write_resp flow resp

  let handler ~addr ~port ~certchains callback flow _ =
    let certs = load_certs certchains in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "start_server"
    in
    let server =
      Tls_eio.server_of_flow (Tls.Config.server ~certificates ()) flow
    in
    Tls_eio.epoch server |> handle_client ~addr ~port callback server

  let run ?(port = 1965) ?(backlog = 10) ?(addr = Net.Ipaddr.V4.loopback)
      ~certchains net callback =
    Switch.run (fun sw ->
        let socket =
          Net.listen ~reuse_addr:true ~backlog ~sw net (`Tcp (addr, port))
        in
        let rec loop () =
          handler ~addr ~port ~certchains callback
          |> Net.accept_fork ~sw ~on_error:raise socket;
          loop ()
        in
        loop ())
end
