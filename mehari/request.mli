(** Client request encapsulation. *)

type t

val uri : t -> Uri.t
val target : t -> string
val ip : t -> Ipaddr.t
val port : t -> int
val sni : t -> string
val query : t -> string option
val client_cert : t -> X509.Certificate.t option

val make :
  ?client_cert:X509.Certificate.t ->
  uri:Uri.t ->
  addr:Ipaddr.t ->
  server_addr:Ipaddr.t ->
  port:int ->
  sni:string ->
  unit ->
  t

val attach_params : t -> Re.Group.t option -> t
val param : t -> int -> string
