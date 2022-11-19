(** Mehari is a {{:https://mirageos.org/ }Mirage OS} friendly library for
building Gemini servers. It fully implements the
{{:https://gemini.circumlunar.space/docs/specification.gmi }Gemini protocol specification}
and aims to expose a clean and simple API.

This module provides the core abstraction, it does not depend on any platform
code, and does not interact with the environment. Input and output are provided
by {!Mehari_unix}.

It takes heavy inspiration from {{: https://github.com/aantron/dream }Dream},
a tidy, feature-complete Web framework. *)

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
    | Preformat of { alt : string option; text : string }
    | Heading of [ `H1 | `H2 | `H3 ] * string
    | ListItem of string
    | Quote of string

  (** {1 Facilities} *)

  val text : string -> line
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

(** {1:response Response} *)

val response : 'a status -> 'a -> response
(** Creates a new {!type:response} with given {!type:Mehari.status}. *)

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

val respond_document : ?mime:mime -> string -> response Lwt.t
(** Same as {!val:respond} but respond with content of given [filename] and use
    given {!type:mime} as mime type.
    If [filename] is not present on filesystem, responds with {!val:not_found}.
    {!type:mime} type is chosen according to the filename extension by default.
    If mime type inference failed, it uses [text/gemini; charset=utf-8]. *)

(** {1:status Status} *)

(** A wrapper around Gemini status codes.

  @see < https://gemini.circumlunar.space/docs/specification.gmi >
    Section "Appendix 1. Full two digit status codes" for a description of the
    meaning of each code. *)

val input : string status
val sensitive_input : string status
val success : body -> mime status
val redirect_temp : string status
val redirect_permanent : string status
val temporary_failure : string status
val server_unavailable : string status
val cgi_error : string status
val proxy_error : string status
val slow_down : int -> string status
val permanent_failure : string status
val not_found : string status
val gone : string status
val proxy_request_refused : string status
val bad_request : string status
val client_certificate_required : string status
val certificate_not_authorised : string status
val certificate_not_valid : string status

(** {1:body Body} *)

val text : string -> body
(** Creates a {!type:body} from given text. *)

val gemtext : Gemtext.t -> body
(** Creates a {!type:body} from a Gemtext document. *)

val lines : string list -> body
(** Creates a {!type:body} from Gemtext line as text. *)

val page : title:string -> string -> body
(** [page ~title content] creates a simple Gemtext {!type:body} of form:

{[
  # title

  content
]}
*)

(** {1:mime Mime} *)

val make_mime : ?charset:string -> ?lang:string list -> string -> mime
(** [make_mime ~charset ~lang mime] creates a {!type:mime} type from given
    [charset] and [lang]s. Charset defaults to [utf-8] if mime type begins with
    [text/]. *)

val from_filename : ?charset:string -> ?lang:string list -> string -> mime
(** [from_filename ~charset ~lang filename] creates a {!type:mime} type by
    performing a mime lookup from [filename]. *)

val empty : mime
(** The empty mime. *)

val gemini : mime
(** [text/gemini; charset=utf-8] *)

val text_mime : string -> mime
(** [text_mime type] returns [text/type; charset=utf-8]. *)

val with_charset : mime -> string -> mime
(** Changes charset of given {!type:mime}. *)

val with_lang : mime -> string list -> mime
(** Changes langs of given {!type:mime}. *)

val with_mime : mime -> string -> mime
(** Changes mime type of given {!type:mime}. *)

(** {1 IO} *)

(** Module type containing all environment-dependent functions. An
    implementation is provided by {!Mehari_unix}. *)
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

  (** {1:routing Routing} *)

  val router : route list -> handler
  (** Creates a router. If none of the routes match the {!type:Mehari.request},
      the router returns {!val:Mehari.not_found}. *)

  val route :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> handler -> route
  (** [route ~rate_limit ~mw path handler] forwards requests for [path] to
      [handler]. If rate limit is in effect, [handler] is not executed and a
      respond with {!type:Mehari.status} {!val:Mehari.slow_down} is sended. *)

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

(** A functor building an IO implementation. Mainly for compatibility with
    Mirage OS. *)
module Make : functor
  (Clock : Mirage_clock.PCLOCK)
  (KV : Mirage_kv.RO)
  (Stack : Tcpip.Stack.V4V6)
  -> IO with type stack = Stack.TCP.t
