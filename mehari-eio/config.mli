(** Eio dependent configuration. *)

type t = { backlog : int; addr : Eio.Net.Ipaddr.v4v6 }

val make : ?backlog:int -> ?addr:Eio.Net.Ipaddr.v4v6 -> unit -> t
