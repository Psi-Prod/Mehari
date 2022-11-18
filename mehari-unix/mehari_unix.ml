include Mehari.Make (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)

module Make_server
    (Clock : Mirage_clock.PCLOCK)
    (KV : Mirage_kv.RO)
    (Stack : Tcpip.Stack.V4V6) =
struct
  (*module TLS = Tls_mirage.Make (Stack.TCP)
    module X509 = Tls_mirage.X509 (KV) (Clock)*)

  module Server = Mehari.Server_impl.Make (Clock) (KV) (Stack)

  let stack =
    Tcpip_stack_socket.V4V6.TCP.connect ~ipv4_only:false ~ipv6_only:false ipaddr
      None

  let run_lwt ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ])
      ipaddr callback =
    Server.start_server ~port ~certchains ~stack callback |> Lwt_main.run

  let run ?(port = 1965) ?(certchains = [ ("./cert.pem", "./key.pem") ]) ipaddr
      callback =
    Server.start_server ~port ~certchains ~stack callback
end

include Make_server (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)
