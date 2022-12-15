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

module Make (Logger : Private.Logger_impl.S) : S with type stack := Eio.Net.t =
struct
  module IO = Direct

  type handler = Private.Handler.Make(IO).t

  let rec handle_connection callback sock =
    let handler flow _ =
      flow#write
        [
          (Cstruct.of_string @@ callback
          @@
          let r = Eio.Buf_read.of_flow flow ~max_size:1_000_000 in
          Eio.Buf_read.line r);
        ]
    in
    let on_error e = raise e in
    Eio.Switch.run @@ fun sw ->
    Eio.Net.accept_fork ~sw ~on_error sock handler;
    handle_connection callback sock

  let run ?(backlog = 10) ?(address = Eio.Net.Ipaddr.V4.loopback) ?(port = 1965)
      stack callback =
    ( Eio.Switch.run @@ fun sw ->
      Eio.Net.listen ~backlog ~sw stack (`Tcp (address, port)) )
    |> handle_connection callback
end
