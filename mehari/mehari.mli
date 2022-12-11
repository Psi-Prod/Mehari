(** Mehari is a {{:https://mirageos.org/ }Mirage OS} friendly library for
building Gemini servers. It fully implements the
{{:https://gemini.circumlunar.space/docs/specification.gmi }Gemini protocol specification}
and aims to expose a clean and simple API.


It takes heavy inspiration from {{: https://github.com/aantron/dream }Dream},
a tidy, feature-complete Web framework.

This module provides the core abstraction, it does not depend on any platform
code, and does not interact with the environment. Input and output are provided
by {!Mehari_unix}. *)

(** {1 Types} *)

type request
(** Gemini request. See {!section-request}. *)

type response
(** Gemini response. See {!section-response}. *)

type handler = request -> response Lwt.t
(** Handlers are asynchronous functions from {!type:request} to
    {!type:response}. *)

type 'a status
(** Status of a Gemini response. See {!section-status}. *)

type mime
(** Mime type of a document. See {!section-mime}. *)

type body
(** Body of Gemini response. See {!section-body}. *)

(** {1:gemtext Gemtext} *)

(** Implementation of the Gemini own native response format. *)
module Gemtext : sig
  type t = line list

  and line =
    | Text of string
    | Link of { url : string; name : string option }
    | Preformat of preformat
    | Heading of [ `H1 | `H2 | `H3 ] * string
    | ListItem of string
    | Quote of string

  and preformat = { alt : string option; text : string }

  val of_string : string -> t
  val to_string : t -> string

  (** {1 Facilities} *)

  val text : string -> line

  val newline : line
  (** [newline] is [text ""]. *)

  val link : ?name:string -> string -> line
  val preformat : ?alt:string -> string -> line
  val heading : [ `H1 | `H2 | `H3 ] -> string -> line
  val list_item : string -> line
  val quote : string -> line
end

(** {1:request Request} *)

val uri : request -> Uri.t
(** Request uri. *)

val ip : request -> Ipaddr.t
(** Address of client sending the {!type:request}. *)

val port : request -> int
(** Port of client sending the {!type:request}. *)

val sni : request -> string option
(** Server name indication TLS extension. *)

val query : request -> string option
(** User uri query. *)

val param : request -> int -> string
(** [param req n] retrieves the [n]-th path parameter of [req].

    @raise Invalid_argument if [n] is not a positive integer or path does not
      contain any parameters in which case the program is buggy. *)

(** {1:response Response} *)

val response : 'a status -> 'a -> response
(** Creates a new {!type:response} with given {!type:Mehari.status}.

    @raise Invalid_argument if [meta] is more than 1024 bytes. *)

val respond : 'a status -> 'a -> response Lwt.t
(** Same as {!val:response}, but the new {!type:response} is wrapped in a
    [Lwt] promise. *)

val respond_body : body -> mime -> response Lwt.t
(** Same as {!val:respond} but respond with given {!type:body} and
    use given {!type:mime} as mime type. *)

val respond_text : string -> response Lwt.t
(** Same as {!val:respond} but respond with given text and use [text/plain] as
    {!type:mime} type. *)

val respond_gemtext : Gemtext.t -> response Lwt.t
(** Same as {!val:respond} but respond with given {!type:Gemtext.t} and use
    [text/gemini] as {!type:mime} type. *)

val raw_response :
  [ `Body of string | `Full of int * string * string ] -> response
(** [raw_response raw] creates a new {!type:response} depending of the value of
    [raw]:.
    - [`Body body]: creates a {!val:respond} with [body].
      No check is performed on bytes-length of header;
    - [`Full (code, meta, body)]: creates a {!val:respond} with given arguments. *)

val raw_respond :
  [ `Body of string | `Full of int * string * string ] -> response Lwt.t
(** Same as {!val:raw_response}, but the new {!type:response} is wrapped in a
    [Lwt] promise. *)

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

val text : string -> body
(** Creates a {!type:body} from given text. *)

val gemtext : Gemtext.t -> body
(** Creates a {!type:body} from a Gemtext document. *)

val lines : string list -> body
(** Creates a {!type:body} from Gemtext line as text. *)

val stream : string Lwt_stream.t -> body
(** Creates a {!type:body} from a stream. *)

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
  [text/]. *)

val from_filename : ?charset:string -> ?lang:string list -> string -> mime
(** [from_filename ?charset ?lang fname] creates a
    {!type:mime} type by performing a mime lookup based on file extension of
    [fname], if it fails returns [make_mime ?charset ?lang "text/gemini"]. *)

val from_content : ?charset:string -> ?lang:string list -> string -> mime option
(** [from_content ?default ?charset ?lang c] tries to create a {!type:mime}
    type by performing a mime lookup based on content [c]. *)

val empty : mime
(** The empty mime. *)

val gemini : mime
(** [gemini] is [text/gemini; charset=utf-8] *)

val text_mime : string -> mime
(** [text_mime type] is [text/type; charset=utf-8]. *)

val with_charset : mime -> string -> mime
(** Changes charset of given {!type:mime}. *)

(** {1 IO} *)

(** Module type containing all environment-dependent functions. An
    implementation for Unix is provided by {!Mehari_unix}. *)
module type IO = sig
  type middleware = handler -> handler
  (** Middlewares take a {!type:Mehari.handler}, and run some code before or
      after — producing a “bigger” {!type:Mehari.handler}. *)

  type route
  (** Routes tell {!val:router} which handler to select for each request. See
      {!section-routing}. *)

  type stack
  (** Tcpip stack. *)

  (** {1:rate_limit Rate limit} *)

  type rate_limiter
  (** Rate limiter. *)

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

  val logger : middleware
  (** Logs and times requests. *)

  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log

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

  (** {1 Entry point} *)

  val run_lwt :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit Lwt.t
  (** [run ?port ?certchains stack handler] runs the server using
      [handler].
      - [port] is the port to listen on. Defaults to [1965].
      - [certchains] is the list of form [[(cert_path, privatekey_path); ...]],
        the last one is considered default.

  @raise Invalid_argument if [certchains] is empty. *)
end

(** Mirage OS compatiblity. *)
module Mirage : sig
  (** A functor building an IO module. *)
  module Make : functor
    (Clock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    -> IO with type stack = Stack.t
end
