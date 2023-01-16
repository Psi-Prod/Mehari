open Lwt.Syntax
include Mehari_io

let response_document = File.respond_document
let static = File.static
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

let default_ipv4 = Ipaddr.V4.Prefix.make 8 Ipaddr.V4.localhost

let run_lwt ?port ?timeout ?verify_url_host ?config ?certchains
    ?(v4 = default_ipv4) ?v6 callback =
  Mirage_crypto_rng_unix.initialize ();
  let* stack = stack ~v4 ~v6 in
  run ?port ?timeout ?verify_url_host ?config ?certchains stack callback

let run ?port ?timeout ?verify_url_host ?config ?certchains ?v4 ?v6 callback =
  run_lwt ?port ?timeout ?verify_url_host ?config ?certchains ?v4 ?v6 callback
  |> Lwt_main.run
