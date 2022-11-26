(*
open Lwt.Syntax
include Mehari.Make (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)
*)
include Mehari.TempMake (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)

(*let stack ~v4 ~v6 =
    Tcpip_stack_socket.V4V6.TCP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6

  let run_lwt ?port ?certchains ?v4 ?v6 callback =
    let* stack =
      stack
        ~v4:
          (match v4 with
          | Some ip -> Ipaddr.V4.Prefix.of_string_exn ip
          | None -> Ipaddr.V4.Prefix.loopback)
        ~v6:(Option.map Ipaddr.V6.Prefix.of_string_exn v6)
    in
    run_lwt ?port ?certchains stack callback

  let run ?port ?certchains ?v4 ?v6 callback =
    run_lwt ?port ?certchains ?v4 ?v6 callback |> Lwt_main.run*)

let run_lwt ?port ?certchains ?v4 ?v6:_ callback =
  run_lwt ?port ?certchains (Option.value ~default:"127.0.0.1" v4) callback

let run ?port ?certchains ?v4 ?v6 callback =
  run_lwt ?port ?certchains ?v4 ?v6 callback |> Lwt_main.run
