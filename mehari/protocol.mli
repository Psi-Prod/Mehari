(** Request validation. *)

type err =
  | AboveMaxSize  (** Request has a size higher than 1024 bytes. *)
  | BeginWithBOM  (** The request begin with a U+FEFF byte order mark. *)
  | EmptyURL  (** URL is empty. *)
  | InvalidURL  (** Invalid URL. *)
  | MalformedUTF8  (** URL contains non-UTF8 byte sequence. *)
  | MissingHost  (** The host URL subcomponent is required.*)
  | MissingScheme  (** URL has no scheme. *)
  | NotADomainName  (** The host URL component is not a valid domain name. *)
  | RelativePath  (** URL path is relative. *)
  | SNIExtRequired  (** SNI extension to TLS is required. *)
  | UserInfoNotAllowed
      (** URL contains userinfo subcomponent which is not allowed. *)
  | WrongHost  (** URL contains a foreign hostname. *)
  | WrongPort  (** URL has an incorrect port number. *)
  | WrongScheme  (** URL scheme is not gemini://. *)

val make_request :
  client_addr:Ipaddr.t ->
  server_addr:Ipaddr.t ->
  ?hostname:[ `host ] Domain_name.t ->
  port:int ->
  verify_url_host:bool ->
  tls_version:Tls.Core.tls_version ->
  ?client_cert:X509.Certificate.t ->
  client_request:string ->
  X509.Certificate.t list ->
  (Request.t, err) result
(** Perform some static check on client request *)

val to_response : err -> Response.t
val equal_err : err -> err -> bool
val pp_err : Format.formatter -> err -> unit
