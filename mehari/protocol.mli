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
  addr:Ipaddr.t ->
  server_addr:Ipaddr.t ->
  verify_url_host:bool ->
  X509.Certificate.t list ->
  Tls.Core.epoch_data ->
  string ->
  (Request.t, request_err) result

val to_response : request_err -> Response.t
