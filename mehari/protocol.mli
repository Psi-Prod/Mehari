(** Client request validation. *)

type err =
  | Above_max_size  (** Request has a size higher than 1024 bytes. *)
  | Begin_with_BOM  (** The request begin with a U+FEFF byte order mark. *)
  | Empty_URL  (** URL is empty. *)
  | Invalid_client_cert of X509.Validation.ca_error
      (** Provided client certificate is not valid. *)
  | Invalid_domain_name
      (** The host URL component is not a valid domain name nor an IP address.
      *)
  | Invalid_URL  (** Invalid URL. *)
  | Malformed_UTF8  (** URL contains non-UTF8 byte sequence. *)
  | Missing_host  (** The host URL subcomponent is required.*)
  | Missing_scheme  (** URL has no scheme. *)
  | Relative_path  (** URL path is relative. *)
  | User_info_not_allowed
      (** URL contains userinfo subcomponent which is not allowed. *)
  | Wrong_port  (** URL has an incorrect port number. *)
  | Wrong_scheme  (** URL scheme is not gemini://. *)

val make_request :
  client:Ipaddr.t * int ->
  ?hostname:[ `host ] Domain_name.t ->
  port:int ->
  tls_version:Tls.Core.tls_version ->
  ?client_cert:X509.Certificate.t ->
  client_request:string ->
  now:Ptime.t ->
  unit ->
  (Request.t, err) result
(** Perform some static check on client request.

    - [client] is the client IP address and its port number.
    - [hostname] is the hostname of the server which received the request.
    - [port] is the port of listening socket where request was received.
    - [tls_version] is the used TLS version (musts be >= 1.2).
    - [client_cert] is the certificate provided by client, if any.
    - [client_request] is the client request, without the trailing [\r\n].
    - [now] is the time at which the reply was received. *)

val to_response : err -> Response.t
(** Transformes an error into a response. *)

val equal_err : err -> err -> bool
val pp_err : Format.formatter -> err -> unit
