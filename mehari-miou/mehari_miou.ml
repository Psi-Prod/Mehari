module Runtime = struct
  type stack = {
    ip : Ipaddr.Prefix.t;
    backlog : int;
    reuseaddr : bool;
    reuseport : bool;
  }

  type listener = Miou_unix.file_descr

  let listen { ip; backlog; reuseaddr; reuseport } port =
    let socket, addr =
      let open Ipaddr in
      match ip with
      | V4 ip -> (Miou_unix.tcpv4 (), V4 V4.(Prefix.address ip))
      | V6 ip -> (Miou_unix.tcpv6 (), V6 V6.(Prefix.address ip))
    in
    let sockaddr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr addr, port) in
    Miou_unix.bind_and_listen ~backlog ~reuseaddr ~reuseport socket sockaddr;
    socket

  module TLS = struct
    include Tls_miou_unix

    let peer fd =
      let[@warning "-8"] (Unix.ADDR_INET (addr, port)) =
        Tls_miou_unix.file_descr fd
        |> Miou_unix.to_file_descr |> Unix.getpeername
      in
      (Ipaddr_unix.of_inet_addr addr, port)

    let really_read fd ?off ?len buf =
      try Ok (really_read fd ?off ?len buf) with
      | Tls_alert a -> Error (`Tls_alert a)
      | Tls_failure f -> Error (`Tls_failure f)

    let write flow ?off ?len s =
      try Ok (write flow ?off ?len s)
      with Closed_by_peer -> Error `Connection_closed
  end

  module TCP = struct
    type t = Miou_unix.file_descr

    let accept _ socket = fst (Miou_unix.accept socket)
    let tls_upgrade config fd = Tls_miou_unix.server_of_fd config fd
  end

  let now = Mirage_ptime.now
  let sleep = Miou_unix.sleep
end

include Static
include Cgi
module Server = Mehari.Server.Make (Runtime)

let run ?timeout ?port ?(reuseport = true) ?(reuseaddr = true) ?(backlog = 64)
    ~certs ip handler =
  let config = { Runtime.ip; backlog; reuseaddr; reuseport } in
  Server.run ?timeout ?port ~certs config handler
