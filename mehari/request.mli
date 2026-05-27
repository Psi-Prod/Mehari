(** Gemini client request. *)

type t

val uri : t -> Uri.t
(** Request uri. *)

val target : t -> string
(** Path of requested URL. For example, "/foo/bar". *)

val ip : t -> Ipaddr.t
(** Address of the client who sent the request. *)

val port : t -> int
(** Port of the requested URL. *)

val query : t -> string option
(** User URI query, if presents. *)

val client_cert : t -> X509.Certificate.t option
(** User client certificates, if provided. *)

val tls_version : t -> [ `TLS_1_2 | `TLS_1_3 ]
(** TLS version used for this request. *)

(**/**)

module Private : sig
  type hostname = Domain_name of [ `host ] Domain_name.t | Ip_addr of Ipaddr.t

  val make :
    ?client_cert:X509.Certificate.t ->
    uri:Uri.t ->
    client:Ipaddr.t * int ->
    port:int ->
    server_hostname:hostname ->
    tls_version:[ `TLS_1_2 | `TLS_1_3 ] ->
    unit ->
    t
  (** Creates a request from given parameters.

      - [client_cert] is the certificate provided by client, if any.
      - [uri] is the URI requested by client.
      - [client_ip] is the client IP address.
      - [client_port] is the client port number.
      - [port] is the port of listening socket where request was received.
      - [server_hostname] is the hostname of the server which received the
        request.
      - [tls_version] is the used TLS version. *)

  val client_port : t -> int
  (* Client port. *)

  val server_hostname : t -> hostname
  (** Server domain name or its IP address if missing. *)
end

(**/**)
