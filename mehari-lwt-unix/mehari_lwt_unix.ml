open Lwt.Syntax
module Stack = Tcpip_stack_socket.V4V6
include Mehari_mirage.Make (Pclock) (Stack) (Time)

let respond_document = File.respond_document
let static = File.static
let run_cgi = File.run_cgi

type config = ips

and ips =
  | IPv4 of Ipaddr.V4.Prefix.t
  | IPv6 of Ipaddr.V6.Prefix.t
  | IPv4v6 of Ipaddr.V4.Prefix.t * Ipaddr.V6.Prefix.t

let config ips = ips
let default_ipv4 = Ipaddr.V4.Prefix.make 8 Ipaddr.V4.localhost
let default_config = IPv4 default_ipv4

let configure_stack ips =
  let ipv4_only, ipv6_only, ipv4, ipv6 =
    match ips with
    | IPv4 ipv4 -> (true, false, ipv4, None)
    | IPv6 ipv6 -> (false, true, default_ipv4, Some ipv6)
    | IPv4v6 (ipv4, ipv6) -> (false, false, ipv4, Some ipv6)
  in
  let* tcp = Stack.TCP.connect ~ipv4_only ~ipv6_only ipv4 ipv6 in
  let* udp = Stack.UDP.connect ~ipv4_only ~ipv6_only ipv4 ipv6 in
  Stack.connect udp tcp

let run ?port ?verify_url_host ?timeout ?(config = default_config) ~certs
    handler =
  Mirage_crypto_rng_unix.use_default ();
  let* stack = configure_stack config in
  run ?port ?timeout ?verify_url_host ~certs stack handler
