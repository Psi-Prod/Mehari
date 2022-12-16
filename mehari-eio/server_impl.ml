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

  let handle_client _callback flow _addr =
    let client_req = Read.of_flow flow ~max_size:1024 in
    Eio.traceln "Received: %S" (Read.line client_req);
    Eio.Flow.copy_string "20 text/gemini" flow

  let run ?(backlog = 10) ?(addr = Eio.Net.Ipaddr.V4.loopback) ?(port = 1965)
      net callback =
    Eio.Switch.run (fun sw ->
        let socket = Eio.Net.listen ~reuse_addr:true ~backlog ~sw net (`Tcp (addr, port)) in
        let rec serve () =
          handle_client callback
          |> Eio.Net.accept_fork ~sw ~on_error:raise socket;
          serve ()
        in
        serve ())
end
