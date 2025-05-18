open Eio

type t = { backlog : int; addr : Net.Ipaddr.v4v6 }

let make ?(backlog = 4096) ?(addr = Net.Ipaddr.V4.loopback) () =
  { backlog; addr }

let default = make ()
