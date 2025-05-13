(** Gemini client request. *)

type t

val uri : t -> Uri.t
(** Request uri. *)

val target : t -> string
(** Path of requested URL. For example, "/foo/bar". *)

val ip : t -> Ipaddr.t
(** Address of client sending the request. *)

val port : t -> int
(** Port of requested URL. *)

val query : t -> string option
(** User URI query, if presents. *)

val client_cert : t -> X509.Certificate.t option
(** User client certificates, if provided. *)

val tls_version : t -> [ `TLS_1_2 | `TLS_1_3 ]
(** TLS version used for this request. *)

val param : t -> int -> string
(** [param req n] retrieves the [n]-th path parameter of [req].

    @raise Invalid_argument if [n] is not a positive integer

    @raise Invalid_argument
      if path does not contain any parameters in which case the program is
      buggy. *)

(**/**)

module Private : sig
  val make :
    ?client_cert:X509.Certificate.t ->
    uri:Uri.t ->
    client_ip:Ipaddr.t ->
    port:int ->
    sni:[ `host ] Domain_name.t ->
    tls_version:[ `TLS_1_2 | `TLS_1_3 ] ->
    unit ->
    t

  val sni : t -> [ `host ] Domain_name.t
  val attach_params : t -> Re.Group.t option -> t
end

(**/**)
