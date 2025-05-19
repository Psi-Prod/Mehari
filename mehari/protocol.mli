(** Client request validation. *)

type err =
  | AboveMaxSize  (** Request has a size higher than 1024 bytes. *)
  | BeginWithBOM  (** The request begin with a U+FEFF byte order mark. *)
  | ClientCertificateNotValid of X509.Validation.ca_error
      (** Provided client certificate is not valid. *)
  | EmptyURL  (** URL is empty. *)
  | InvalidURL  (** Invalid URL. *)
  | MalformedUTF8  (** URL contains non-UTF8 byte sequence. *)
  | MissingHost  (** The host URL subcomponent is required.*)
  | MissingScheme  (** URL has no scheme. *)
  | NotADomainName
      (** The host URL component is not a valid domain name nor an IP address.
      *)
  | RelativePath  (** URL path is relative. *)
  | UserInfoNotAllowed
      (** URL contains userinfo subcomponent which is not allowed. *)
  | WrongPort  (** URL has an incorrect port number. *)
  | WrongScheme  (** URL scheme is not gemini://. *)

val make_request :
  client_ip:Ipaddr.t ->
  client_port:int ->
  ?hostname:[ `host ] Domain_name.t ->
  port:int ->
  tls_version:Tls.Core.tls_version ->
  ?client_cert:X509.Certificate.t ->
  client_request:string ->
  now:Ptime.t ->
  unit ->
  (Request.t, err) result
(** Perform some static check on client request.

    - [client_ip] is the client IP address.
    - [client_ip] is the client port number.
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
