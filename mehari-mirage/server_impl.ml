open Mehari

module type S = sig
  type stack

  module IO : Private.IO

  type handler = Ipaddr.t Private.Handler.Make(IO).t

  val run_lwt :
    ?addr:Ipaddr.t ->
    ?port:int ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t

  val run :
    ?addr:Ipaddr.t ->
    ?port:int ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit
end

module Make (Stack : Tcpip.Stack.V4V6) (Logger : Private.Logger_impl.S) :
  S with module IO = Lwt and type stack := Stack.t = struct
  module IO = Lwt

  type handler = Ipaddr.t Private.Handler.Make(IO).t

  module TLS = Tls_mirage.Make (Stack.TCP)
  module Channel = Mirage_channel.Make (TLS)
  open Lwt.Syntax

  let load_certs certs =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | (cert, priv_key) :: tl ->
          let* certchain = X509_lwt.private_of_pems ~cert ~priv_key in
          aux (certchain :: acc) tl
    in
    aux [] certs

  let write chan buf = Channel.write_line chan buf

  let write_resp chan resp =
    (match Mehari.Private.view_of_resp resp with
    | Immediate [] -> ()
    | Immediate (hd :: tl) ->
        List.iter (write chan) ([ String.sub hd 0 (String.length hd - 1) ] @ tl)
    | Delayed d -> d (write chan));
    match%lwt Channel.flush chan with
    | Ok () -> Lwt.return_unit
    | Error _ -> failwith "writing"

  let read flow =
    match%lwt Channel.read_some flow with
    | Ok (`Data buffer) -> Cstruct.to_string buffer |> Lwt.return
    | Ok `Eof -> failwith "eof"
    | Error _ -> failwith "reading"

  let client_req = Re.(compile (seq [ group (rep1 any); char '\r'; char '\n' ]))

  let handle_client ~addr ~port ~timeout:_ callback flow ep =
    (* TODO: Implement timeout here too *)
    let chan = Channel.create flow in
    let* request = read chan in
    let* resp =
      match Re.exec_opt client_req request with
      | None -> (response bad_request) "" |> Lwt.return
      | Some grp ->
          let uri = Re.Group.get grp 1 |> Uri.of_string in
          let sni =
            match ep with
            | Ok data -> Option.map Domain_name.to_string data.Tls.Core.own_name
            | Error () -> assert false
          in
          let client_cert =
            match ep with
            | Ok data -> Option.to_list data.Tls.Core.peer_certificate
            | Error () -> assert false
          in
          Private.make_request
            (module Ipaddr)
            ~addr ~port ~uri ~sni ~client_cert
          |> callback
    in
    let* () = write_resp chan resp in
    TLS.close flow

  let handler ~timeout ~certificates ~config ~addr ~port callback flow =
    let* server_r =
      TLS.server_of_flow
        (match config with
        | Some c -> c
        | None ->
            Tls.Config.server ~certificates
              ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
              ())
        flow
    in
    let server =
      match server_r with Ok s -> s | Error _ -> failwith "server_of_flow"
    in
    TLS.epoch server |> handle_client ~timeout ~addr ~port callback server

  let run_lwt ?(addr = Ipaddr.V4 Ipaddr.V4.localhost) ?(port = 1965) ?config
      ?(certchains = [ ("./cert.pem", "./key.pem") ]) stack callback =
    let* certs = load_certs certchains in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "Mehari_eio.run"
    in
    handler ~timeout:() ~certificates ~config ~addr ~port callback
    |> Stack.TCP.listen (Stack.tcp stack) ~port;
    Stack.listen stack

  let run ?addr ?port ?config ?certchains stack callback =
    run_lwt ?port ?addr ?config ?certchains stack callback |> Lwt_main.run
end
