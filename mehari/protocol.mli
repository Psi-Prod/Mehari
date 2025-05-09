(** Request validation. *)

type request_err =
  | AboveMaxSize
  | BeginWithBOM
  | EmptyURL
  | InvalidURL
  | MalformedUTF8
  | MissingHost
  | MissingScheme
  | NotADomainName
  | RelativePath
  | SNIExtRequired
  | UserInfoNotAllowed
  | WrongHost
  | WrongPort
  | WrongScheme

val make_request :
  port:int ->
  client_addr:Ipaddr.t ->
  server_addr:Ipaddr.t ->
  hostname:[ `host ] Domain_name.t option ->
  verify_url_host:bool ->
  tls_version:Tls.Core.tls_version ->
  client_cert:X509.Certificate.t option ->
  client_request:string ->
  X509.Certificate.t list ->
  (Request.t, request_err) result

val to_response : request_err -> Response.t
