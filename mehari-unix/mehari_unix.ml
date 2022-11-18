open Lwt.Syntax
include Mehari.Make (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)

let stack =
  Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false
    Ipaddr.V4.Prefix.loopback None

let run_lwt =
  let* stack in
  run_lwt stack
