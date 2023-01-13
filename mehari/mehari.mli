(** This module provides the core abstraction, it does not depend on any platform
code, and does not interact with the environment. *)

(** {1 Types} *)

type 'addr request
(** Gemini request. See {!section-request}. *)

type response
(** Gemini response. See {!section-response}. *)

type 'a status
(** Status of a Gemini response. See {!section-status}. *)

type mime
(** Mime type of a document. See {!section-mime}. *)

type body
(** Body of Gemini response. See {!section-body}. *)

(** {1:gemtext Gemtext} *)

module Gemtext : sig
  (** Implementation of the Gemini own native response format.
      Note that if a string containing line breaks ([CR] or [CRLF]) is given
      to functions {!val:heading}, {!val:list_item} and {!val:quote} only the
      first line will be formatted and the others treated as normal text.
      To avoid this behavior, see {!val:Mehari.paragraph}.

      {@ocaml[open Mehari.Gemtext

let () = assert ([ quote "hello\nworld" ] = [ quote "hello"; text "world" ])
]} *)

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
  val pp : Format.formatter -> t -> unit
end

val paragraph : (string -> Gemtext.line) -> string -> Gemtext.t
(** [paragraph to_gemtext str] is a convenient function to transform a string
    containing line breaks ([CR] or [CRLF]) into a Gemtext document.

    {@ocaml[open Mehari.Gemtext

let () = assert (Mehari.paragraph quote "hello\nworld" = [ quote "hello"; quote "world" ])
]} *)

(** {1:request Request} *)

val uri : 'a request -> Uri.t
(** Request uri. *)

val target : 'a request -> string
(** Path of requested URL. For example, "/foo/bar". *)

val ip : 'addr request -> 'addr
(** Address of client sending the {!type:request}. *)

val port : 'a request -> int
(** Port of client sending the {!type:request}. *)

val sni : 'a request -> string
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
      Section "Appendix 1. Full two digit status codes" for a description of
      the meaning of each code. *)

val input : string status
val sensitive_input : string status
val success : body -> mime status
val redirect_temp : string status
val redirect_perm : string status
val temporary_failure : string status
val server_unavailable : string status
val cgi_error : string status
val proxy_error : string status
val slow_down : int status
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

(** {2:note-on-data-stream-response A note on data stream response}

    Mehari offers ways to keep client connections open forever and stream
    data in real time such as {!val:seq} and {!val:stream} functions when the
    [flush] parameter is specified. It is important to note that most Gemini
    clients do not support streaming and should be used with caution. That's
    why this parameter is set to [false] by default in all the functions that
    Mehari expose. *)

val string : string -> body
(** Creates a {!type:body} from given string. *)

val gemtext : Gemtext.t -> body
(** Creates a {!type:body} from a {!type:Gemtext.t} document. *)

val lines : string list -> body
(** Creates a {!type:body} from given lines. Each line is written followed by a
    newline ([LF]) character. *)

val page : title:string -> string -> body
(** [page ~title content] creates a simple Gemtext {!type:body} of form:
{@gemtext[
  # title
  content
]}
*)

val seq : ?flush:bool -> string Seq.t -> body
(** Creates a {!type:body} from a string sequence. See
    {!section:"note-on-data-stream-response"} for a description of [flush]
    parameter. *)

val stream : ?flush:bool -> ((string -> unit) -> unit) -> body
(** [stream (fun consume -> ...)] creates a {!type:body} from a data stream.
    Each call to [consume] write the given input on socket. Useful for stream
    data or file chunk in real time. See
    {!section:"note-on-data-stream-response"} for a description of [flush]
    parameter. *)

(** {1:mime Mime} *)

val make_mime : ?charset:string -> string -> mime
(** [make_mime ?charset mime] creates a {!type:mime} type from given
    [charset]. Charset defaults to [utf-8] if mime type begins with
    [text/].

    @see < https://www.rfc-editor.org/rfc/rfc2046#section-4.1.2 >
      For a description of the "charset" parameter. *)

val from_filename : ?charset:string -> string -> mime option
(** [from_filename ?charset fname] tries to create a {!type:mime} by
    performing a mime lookup based on file extension of [fname].

    Note that mime {!val:gemini} are not infered from files with [.gmi]
    extension. See {{:https://github.com/Psi-Prod/Mehari/issues/36}}. *)

val from_content : ?charset:string -> string -> mime option
(** [from_content ?charset c] tries to create a {!type:mime} type by performing
    a mime lookup based on content [c]. *)

val no_mime : mime
(** Represents the absence of a mime. This is a shortcut for [make_mime ""]. *)

val gemini : ?charset:string -> ?lang:string list -> unit -> mime
(** [gemini ?charset ?lang ()] is [text/gemini; charset=...; lang=...].

    @see < https://www.rfc-editor.org/rfc/rfc2046#section-4.1.2 >
      For a description of the "charset" parameter.

    @see < https://www.ietf.org/rfc/bcp/bcp47.txt >
      For a description of the "lang" parameter. *)

val app_octet_stream : mime
(** [app_octet_stream] is a shortcut for [application/octet-stream]. *)

val plaintext : mime
(** [plaintext] is a shortcut for [text/plain; charset=utf-8]. *)

val text : string -> mime
(** [text type] is a shortcut for [text/type; charset=utf-8]. *)

val with_charset : mime -> string -> mime
(** Set charset of given {!type:mime}. *)

(** {1 IO} *)

(** Module type containing all environment-dependent functions. *)
module type NET = sig
  module IO : Types.IO

  type addr
  (** Type for IP address. *)

  type handler = addr request -> response IO.t
  (** Handlers are asynchronous functions from {!type:Mehari.request} to
      {!type:Mehari.response}. *)

  type route
  (** Routes tell {!val:router} which handler to select for each request. See
      {!section-routing}. *)

  type rate_limiter
  (** Rate limiter. See {!section-rate_limit}. *)

  type middleware = handler -> handler
  (** Middlewares take a {!type:handler}, and run some code before or
      after — producing a “bigger” {!type:handler}. See
      {!section-middleware}. *)

  (** {1:middleware Middleware} *)

  val no_middleware : middleware
  (** Does nothing but call its inner handler. Useful for disabling middleware
      conditionally during application startup:

      {@ocaml[
if development then
  my_middleware
else
  Mehari.no_middleware
]} *)

  val pipeline : middleware list -> middleware
  (** Combines a list of middlewares into one, such that these two lines are
      equivalent: [Mehari.pipeline [ mw1 ; mw2 ] @@ handler]
      [ mw1 @@ mw2 @@ handler]. *)

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

  val no_route : route
  (** A dummy value of type {!type:route} that is completely ignored by the
      router. Useful for disabling routes conditionally during application
      start. *)

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

  (** {1:host Virtual hosting} *)

  val virtual_hosts :
    ?meth:[ `ByURL | `SNI ] -> (string * handler) list -> handler
  (** [virtual_hosts ?meth [(domain, handler); ...]] produces a {!type:handler}
      which enables virtual hosting at the TLS-layer using SNI.
      - [meth] can be used to choose
        which source to match the hostnames against.
        Defaults to [`SNI]. *)

  (** {1 Logging} *)

  val set_log_lvl : Logs.level -> unit
  (** Set Mehari's logger to the given log level. *)

  val logger : handler -> handler
  (** Logs and times requests. Time spent logging is included. *)

  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

(** Module type containing all UNIX-dependent functions. *)
module type UNIX = sig
  module IO : Types.IO

  type addr
  type handler = addr request -> response IO.t
  type dir_path

  (** {1 Static files} *)

  val response_document : ?mime:mime -> dir_path -> response IO.t
  (** Same as {!val:Mehari.response} but respond with content of given
      [filename] and use given {!type:Mehari.mime} as mime type.
      If [filename] is not present on filesystem, responds with
      {!val:Mehari.not_found}. If [mime] parameter is not supplied, use
      {!val:Mehari.no_mime} as mime type. *)

  val static :
    ?handler:(dir_path -> handler) ->
    ?dir_listing:
      (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    dir_path ->
    handler
  (** [static dir] validates the path parameter (retrieved by
  calling [Mehari.param req 1]) by checking that it is relative and does not
  contain parent directory references. If these checks fail,
  responds with {!val:Mehari.not_found}.

  If the checks succeed, [static] calls [handler path request],
  where [path] is the path generated by the concatenation of directory that
  was passed to [static] and path of request. [handler] defaults to
  {!val:response_document}.

  If a directory is requested, [static] will look for a file named
  [index] in that directory to return. Otherwise, a directory file listing
  will be generated by calling [dir_listing [ filename; ... ] request].
  [index] is default on [index.gmi].

  [show_hidden] decides whether hidden files should be listed. It defaults to
  [false] for security reasons. *)
end

(** {1 Private} *)

(** You can ignore it, unless you are porting [Mehari] to a new platform not
    supported by the existing IO backends. *)
module Private : sig
  module type IO = Types.IO
  module type ADDR = Types.ADDR

  type response_view = Response.view

  val view_of_resp : response -> response_view

  module Handler : sig
    module Make (IO : IO) : sig
      type 'addr t = 'addr request -> response IO.t
    end
  end

  module Cert = Cert

  module CGI : sig
    module type S = sig
      type addr

      val make_env :
        addr request -> fullpath:string -> path:string -> string array
    end

    module Make (Addr : sig
      type t

      val compare : t -> t -> int
      val pp : Stdlib.Format.formatter -> t -> unit
    end) : S with type addr := Addr.t
  end

  module Logger_impl : sig
    module type S = sig
      module IO : IO

      type addr
      type handler = addr Handler.Make(IO).t

      val set_level : Logs.level -> unit
      val logger : handler -> handler
      val debug : 'a Logs.log
      val info : 'a Logs.log
      val warning : 'a Logs.log
      val error : 'a Logs.log
    end

    module Make
        (Clock : Mirage_clock.PCLOCK) (IO : sig
          include IO

          val finally : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
        end)
        (Addr : ADDR) : S with module IO = IO and type addr = Addr.t
  end

  module Protocol : sig
    type request_err =
      | AboveMaxSize
      | BeginWithBOM
      | EmptyURL
      | InvalidURL
      | MalformedUTF8
      | MissingHost
      | MissingScheme
      | RelativePath
      | SNIExtRequired
      | UserInfoNotAllowed
      | WrongHost
      | WrongPort
      | WrongScheme

    val make_request :
      (module ADDR with type t = 'a) ->
      port:int ->
      addr:'a ->
      verify_url_host:bool ->
      Tls.Core.epoch_data ->
      string ->
      ('a request, request_err) result

    val to_response : request_err -> response
  end

  module Rate_limiter_impl : sig
    module type S = sig
      module IO : IO

      type t

      module Addr : ADDR

      val check : t -> Addr.t request -> response IO.t option
      val make : ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
    end

    module Make (Clock : Mirage_clock.PCLOCK) (IO : IO) (Addr : ADDR) :
      S with module IO = IO and module Addr = Addr
  end

  module Router_impl : sig
    module type S = sig
      module IO : IO

      type route
      type rate_limiter
      type addr
      type handler = addr Handler.Make(IO).t
      type middleware = handler -> handler

      val no_middleware : middleware
      val pipeline : middleware list -> middleware
      val router : route list -> handler

      val route :
        ?rate_limit:rate_limiter ->
        ?mw:middleware ->
        ?typ:[ `Raw | `Regex ] ->
        string ->
        handler ->
        route

      val scope :
        ?rate_limit:rate_limiter ->
        ?mw:middleware ->
        string ->
        route list ->
        route

      val no_route : route

      val virtual_hosts :
        ?meth:[ `ByURL | `SNI ] -> (string * handler) list -> handler
    end

    module Make (RateLimiter : Rate_limiter_impl.S) (Logger : Logger_impl.S) :
      S
        with module IO = RateLimiter.IO
         and type rate_limiter := RateLimiter.t
         and type addr := RateLimiter.Addr.t
  end

  module Static : sig
    module type DIR = sig
      module IO : Types.IO

      type path

      val kind : path -> [ `Regular_file | `Directory | `Other ] IO.t
      val read : path -> string list IO.t
      val concat : path -> string -> path
      val response_document : ?mime:mime -> path -> response IO.t
      val pp_io_err : Format.formatter -> exn -> unit
    end

    module type S = sig
      module IO : Types.IO

      type addr
      type handler = addr Handler.Make(IO).t
      type dir_path

      val static :
        ?handler:(dir_path -> handler) ->
        ?dir_listing:
          (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
        ?index:string ->
        ?show_hidden:bool ->
        dir_path ->
        handler
    end

    module Make
        (Dir : DIR) (Addr : sig
          type t
        end) :
      S
        with module IO := Dir.IO
         and type addr := Addr.t
         and type dir_path := Dir.path
  end
end
