(** Mehari is a {{:https://mirageos.org/ }Mirage OS} friendly library for
building Gemini servers. It fully implements the
{{:https://gemini.circumlunar.space/docs/specification.gmi }Gemini protocol specification}
and aims to expose a clean and simple API.
It takes heavy inspiration from {{: https://github.com/aantron/dream }Dream},
a tidy, feature-complete Web framework.
This module provides the core abstraction, it does not depend on any platform
code, and does not interact with the environment. Input and output for Unix are
provided by {!Mehari_lwt_unix}. *)

(** {1 Types} *)

type 'addr request = 'addr Request.t
(** Gemini request. See {!section-request}. *)

type response = Response.t
(** Gemini response. See {!section-response}. *)

type 'a status = 'a Response.status
(** Status of a Gemini response. See {!section-status}. *)

type mime = Mime.t
(** Mime type of a document. See {!section-mime}. *)

type body = Response.body
(** Body of Gemini response. See {!section-body}. *)

(** {1:gemtext Gemtext} *)

module Gemtext = Gemtext

(** {1:request Request} *)

val uri : 'a request -> Uri.t
(** Request uri. *)

val ip : 'addr request -> 'addr
(** Address of client sending the {!type:request}. *)

val port : 'a request -> int
(** Port of client sending the {!type:request}. *)

val sni : 'a request -> string option
(** Server name indication TLS extension. *)

val query : 'a request -> string option
(** User uri query. *)

val client_cert : 'a request -> X509.Certificate.t list
(** User client certificates. *)

val param : 'a request -> int -> string
(** [param req n] retrieves the [n]-th path parameter of [req].
    @raise Invalid_argument if [n] is not a positive integer
    @raise Invalid_argument if path does not contain any parameters in which
      case the program is buggy. *)

(** {1:response Response} *)

val response : 'a status -> 'a -> response
(** Creates a new {!type:response} with given {!type:Mehari.status}.
    @raise Invalid_argument if [meta] is more than 1024 bytes.
    @raise Invalid_argument if [meta] starts with [U+FEFF] byte order mark. *)

val response_body : body -> mime -> response
(** Same as {!val:response} but respond with given {!type:body} and
        use given {!type:mime} as mime type. *)

val response_text : string -> response
(** Same as {!val:response} but respond with given text and use [text/plain] as
        {!type:mime} type. *)

val response_gemtext :
  ?charset:string -> ?lang:string list -> Gemtext.t -> response
(** Same as {!val:response} but respond with given {!type:Gemtext.t} and use
        [text/gemini] as {!type:mime} type. *)

val response_raw :
  [ `Body of string | `Full of int * string * string ] -> response
(** Creates a new raw {!type:response}. Does not perform any check on validity
      i.e. length of header or beginning with a byte order mark [U+FEFF].
      - [`Body body]: creates a {!val:response} with [body].
      - [`Full (code, meta, body)]: creates a {!val:response} with given arguments. *)

(** {1:status Status} *)

(** A wrapper around Gemini status codes.
  @see < https://gemini.circumlunar.space/docs/specification.gmi >
    Section "Appendix 1. Full two digit status codes" for a description of the
    meaning of each code. *)

val input : string status
val sensitive_input : string status
val success : body -> mime status
val redirect_temp : string status
val redirect_perm : string status
val temporary_failure : string status
val server_unavailable : string status
val cgi_error : string status
val proxy_error : string status
val slow_down : int -> string status
val perm_failure : string status
val not_found : string status
val gone : string status
val proxy_request_refused : string status
val bad_request : string status
val client_cert_req : string status
val cert_not_authorised : string status
val cert_not_valid : string status

val code_of_status : 'a status -> int
(** [code_of_status s] is status code associated with status [s]. *)

(** {1:body Body} *)

val string : string -> body
(** Creates a {!type:body} from given string. *)

val gemtext : Gemtext.t -> body
(** Creates a {!type:body} from a Gemtext document. *)

val lines : string list -> body
(** Creates a {!type:body} from Gemtext line as text. *)

val seq : string Seq.t -> body
(** Creates a {!type:body} from a string sequence. *)

val delayed : ((string -> unit) -> unit) -> body
(** [delayed (fun consume -> ...)] allows the creation of a {!type:body} with a
    buffering function. Each call to [consume] adds the given input to a
    buffer. Useful for file chunks streaming. *)

val page : title:string -> string -> body
(** [page ~title content] creates a simple Gemtext {!type:body} of form:
{[
  # title
  content
]}
*)

(** {1:mime Mime} *)

val make_mime : ?charset:string -> ?lang:string list -> string -> mime
(** [make_mime?charset ?lang mime] creates a {!type:mime} type from given
  [charset] and [lang]s. Charset defaults to [utf-8] if mime type begins with
  [text/]. [lang] parameter is ignored if [mime] is different from
  "text/gemini".

  @see < https://www.rfc-editor.org/rfc/rfc2046#section-4.1.2 >
    For a description of the "charset" parameter.

  @see < https://www.ietf.org/rfc/bcp/bcp47.txt >
    For a description of the "lang" parameter. *)

val from_filename :
  ?charset:string -> ?lang:string list -> string -> mime option
(** [from_filename ?charset ?lang fname] tries to create a {!type:mime} by
    performing a mime lookup based on file extension of [fname]. *)

val from_content : ?charset:string -> ?lang:string list -> string -> mime option
(** [from_content ?default ?charset ?lang c] tries to create a {!type:mime}
    type by performing a mime lookup based on content [c]. *)

val no_mime : mime
(** Represents the absence of a mime. This is a shortcut for [make_mime ""]. *)

val gemini : ?charset:string -> ?lang:string list -> unit -> mime
(** [gemini] is a shortcut for [make_mime "text/gemini"]. *)

val app_octet_stream : mime
(** [app_octet_stream] is a shortcut for [application/octet-stream]. *)

val plaintext : mime
(** [plaintext] is a shortcut for [text/plain; charset=utf-8]. *)

val text : string -> mime
(** [text type] is [text/type; charset=utf-8]. *)

val with_charset : mime -> string -> mime
(** Set charset of given {!type:mime}. *)

(** {1 IO} *)

(** Module type containing all environment-dependent functions. An
    implementation for Unix using {!Lwt} is provided by {!Mehari_lwt_unix}. *)
module type NET = sig
  module IO : Types.IO

  type route
  (** Routes tell {!val:router} which handler to select for each request. See
      {!section-routing}. *)

  type rate_limiter
  (** Rate limiter. See {!section-rate_limit}. *)

  type addr
  (** Type for IP address. *)

  type handler = addr request -> response IO.t
  (** Handlers are asynchronous functions from {!type:Mehari.request} to
    {!type:Mehari.response}. *)

  type middleware = handler -> handler
  (** Middlewares take a {!type:handler}, and run some code before or
        after — producing a “bigger” {!type:handler}. *)

  (** {1:routing Routing} *)

  val router : route list -> handler
  (** Creates a router. If none of the routes match the {!type:Mehari.request},
      the router returns {!val:Mehari.not_found}. *)

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?typ:[ `Raw | `Regex ] ->
    string ->
    handler ->
    route
  (** [route ~rate_limit ~mw ~typ path handler] forwards requests for [path] to
      [handler]. [path] can be a string literal or a regex in Perl style
      depending of [typ].
      If rate limit is in effect, [handler] is not executed and a respond with
      {!type:Mehari.status} {!val:Mehari.slow_down} is sended. *)

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route
  (** [scope ~rate_limit ~mw prefix routes] groups [routes] under the path
      [prefix], [rate_limit] and [mw]. *)

  (** {1:rate_limit Rate limit} *)

  val make_rate_limit :
    ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> rate_limiter
  (** [make_rate_limit ~period n unit] creates a {!type:rate_limiter} which
      limits client to [n] request per [period * unit].
      For example,
      {[
  make_rate_limit ~period:2 5 `Hour
      ]}
      limits client to 5 requests every 2 hours. *)

  (** {1 Logging} *)

  val set_log_lvl : Logs.level -> unit
  (** Set Mehari's logger to the given log level. *)

  val logger : handler -> handler
  (** Logs and times requests. *)

  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

(** {1 Private} *)

(** You can ignore it, unless you are porting [Mehari] to a new platform not
    supported by the existing IO backends. *)
module Private : sig
  module type IO = Types.IO
  module type ADDR = Types.ADDR

  type response_view = Response.view

  val view_of_resp : response -> response_view

  val make_request :
    (module ADDR with type t = 'addr) ->
    uri:Uri.t ->
    addr:'addr ->
    port:int ->
    sni:string option ->
    client_cert:X509.Certificate.t list ->
    'addr request

  module Handler : sig
    module Make (IO : Types.IO) : sig
      type 'addr t = 'addr request -> response IO.t
    end
  end

  module Cert = Cert
  module CGI = Cgi
  module Logger_impl = Logger_impl
  module Protocol = Protocol
  module Rate_limiter_impl = Rate_limiter_impl
  module Router_impl = Router_impl
end
