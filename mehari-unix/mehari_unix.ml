open Lwt.Syntax
include Mehari.Make (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)

let run_lwt ?port ?certchains callback =
  let* stack =
    Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false
      Ipaddr.V4.Prefix.loopback None
  in
  run_lwt ?port ?certchains stack callback

let run ?port ?certchains callback =
  run_lwt ?port ?certchains callback |> Lwt_main.run
