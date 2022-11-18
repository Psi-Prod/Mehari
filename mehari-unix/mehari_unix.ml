include Mehari.Make (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)

let stack =
  Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false ipaddr
    None

let run_lwt = Mehari.run_lwt ~stack
let run = Mehari.run ~stack
