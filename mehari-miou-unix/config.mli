(** Miou dependent server configuration. *)

type t = {
  ip : Ipaddr.Prefix.t;
  backlog : int;
  reuseaddr : bool;
  reuseport : bool;
}

val make :
  ?reuseport:bool ->
  ?reuseaddr:bool ->
  ?backlog:int ->
  ?ip:Ipaddr.Prefix.t ->
  unit ->
  t
