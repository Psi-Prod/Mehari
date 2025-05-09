(** Client request encapsulation. *)

type t

val uri : t -> Uri.t
val target : t -> string
val ip : t -> Ipaddr.t
val port : t -> int
val sni : t -> string
val query : t -> string option
val client_cert : t -> X509.Certificate.t option
val tls_version : t -> [ `TLS_1_2 | `TLS_1_3 ]

val make :
  ?client_cert:X509.Certificate.t ->
  uri:Uri.t ->
  client_addr:Ipaddr.t ->
  server_addr:Ipaddr.t ->
  port:int ->
  sni:string ->
  tls_version:[ `TLS_1_2 | `TLS_1_3 ] ->
  unit ->
  t

val attach_params : t -> Re.Group.t option -> t
val param : t -> int -> string
