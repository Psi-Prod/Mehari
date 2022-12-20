open Lwt.Syntax
include Mehari_io

let respond_document = File.respond_document
let from_filename = File.from_filename
let run_cgi = File.run_cgi

let stack ~v4 ~v6 =
  let* tcp =
    Stack.TCP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6
  in
  let* udp =
    Stack.UDP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6
  in
  Stack.connect udp tcp

let run_lwt ?port ?certchains ?v4 ?v6 callback =
  let* stack =
    let v4 =
      match v4 with
      | Some ip -> Ipaddr.V4.Prefix.of_string_exn ip
      | None -> Ipaddr.V4.Prefix.loopback
    in
    stack ~v4 ~v6:(Option.map Ipaddr.V6.Prefix.of_string_exn v6)
  in
  run_lwt ?port ?certchains stack callback

let run ?port ?certchains ?v4 ?v6 callback =
  run_lwt ?port ?certchains ?v4 ?v6 callback |> Lwt_main.run
