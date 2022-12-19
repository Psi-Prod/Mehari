open Mehari

module type S = sig
  type stack

  module IO : Private.IO

  type handler = Ipaddr.t Private.Handler.Make(IO).t

  val run_lwt :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t

  val run :
    ?port:int -> ?certchains:(string * string) list -> stack -> handler -> unit
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

  let write chan buf = Channel.write_string chan buf 0 (String.length buf - 1)

  let write_resp chan resp =
    (match Mehari.Private.view_of_resp resp with
    | Immediate bufs -> List.iter (write chan) bufs
    | Delayed d -> d (write chan));
    match%lwt Channel.flush chan with
    | Ok () -> Lwt.return_unit
    | Error _ -> failwith "writing"

  let read flow =
    match%lwt Channel.read_some flow with
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

  let client_req = Re.(compile (seq [ group (rep1 any); char '\r'; char '\n' ]))

  let handle_client callback flow (addr, port) ep =
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
          Private.make_request (module Ipaddr) ~addr ~port ~uri ~sni |> callback
    in
    write_resp chan resp

  let start_server ~port ~certchains ~stack callback =
    let* certs = load_certs certchains in
    let certificates =
      match certs with
      | c :: _ -> `Multiple_default (c, certs)
      | _ -> invalid_arg "start_server"
    in
    handle_client callback
    |> serve (Tls.Config.server ~certificates ())
    |> Stack.TCP.listen (Stack.tcp stack) ~port;
    Stack.listen stack

  let run_lwt ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ])
      stack callback =
    start_server ~port ~certchains ~stack callback

  let run ?port ?certchains stack callback =
    run_lwt ?port ?certchains stack callback |> Lwt_main.run
end
