(** This module provides the core abstraction, it does not depend on any
    platform code, and does not interact with the environment. *)

(** {1 Types} *)

type request = Request.t
(** [request] is an alias for {!type:Request.t}. See {!section-request}. *)

type response = Response.t
(** [response] is an alias for {!type:Response.t}. See {!section-response}. *)

type 'a status = 'a Response.Status.t
(** [status] is an alias for {!type:Response.Status.t}. See {!section-status}.
*)

type mime = Mime.t
(** [mime] is an alias for {!type:Mime.t}. See {!section-mime}. *)

type body = Response.Body.t
(** [body] is an alias for {!type:Response.Body.t}. See {!section-body}. *)

(** {1:gemtext Gemtext} *)

module Gemtext = Gemtext

val paragraph : (string -> Gemtext.line) -> string -> Gemtext.t
(** [paragraph to_gemtext str] is a convenient function to transform a string
    containing line breaks ([CR] or [CRLF]) into a Gemtext document.

    {@ocaml[
      open Mehari.Gemtext

      let () =
        assert (
          Mehari.paragraph quote "hello\nworld"
          = [ quote "hello"; quote "world" ])
    ]} *)

(** {1:request Request} *)

module Request = Request

(** {1:response Response} *)

module Response = Response

(** {1:body Response body} *)

module Body = Response.Body

(** {1:status Response status} *)

module Status = Response.Status

(** {1:mime Mime} *)

module Mime = Mime

(** {1 IO} *)

(** Module type containing all environment-dependent functions. *)
module type NET = sig
  module IO : Signatures.IO

  type handler = request -> response IO.t
  (** Handlers are asynchronous functions from {!type:Mehari.request} to
      {!type:Mehari.response}. *)

  type route
  (** Routes tell {!val:router} which handler to select for each request. See
      {!section-routing}. *)

  type rate_limiter
  (** Rate limiter. See {!section-rate_limit}. *)

  type middleware = handler -> handler
  (** Middlewares take a {!type:handler}, and run some code before or after —
      producing a “bigger” {!type:handler}. See {!section-middleware}. *)

  type clock
  (** System clock used by rate limiter. *)

  (** {1:middleware Middleware} *)

  val no_middleware : middleware
  (** Does nothing but call its inner handler. Useful for disabling middleware
      conditionally during application startup:

      {@ocaml[
        if development then my_middleware else Mehari.no_middleware
      ]} *)

  val pipeline : middleware list -> middleware
  (** Combines a list of middlewares into one, such that these two lines are
      equivalent: [Mehari.pipeline [ mw1 ; mw2 ] @@ handler]
      [ mw1 @@ mw2 @@ handler]. *)

  (** {1:routing Routing} *)

  val router : route list -> handler
  (** Creates a router. If none of the routes match the {!type:Mehari.request},
      the router returns {!val:Mehari.Response.Status.not_found}. *)

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?regex:bool ->
    string ->
    handler ->
    route
  (** [route ~rate_limit ~mw ~regex path handler] forwards requests for [path]
      to [handler]. [path] can be a string literal or a regex in Perl style
      depending of value of [regex]. If rate limit is in effect, [handler] is
      not executed and a respond with {!type:Mehari.status}
      {!val:Mehari.Response.Status.slow_down} is sended. *)

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
    clock ->
    ?period:int ->
    int ->
    [ `Second | `Minute | `Hour | `Day ] ->
    rate_limiter
  (** [make_rate_limit clock ~period n unit] creates a {!type:rate_limiter}
      which limits client to [n] request per [period * unit]. For example,
      {[
        make_rate_limit ~period:2 5 `Hour
      ]}
      limits client to 5 requests every 2 hours. *)

  (** {1:host Virtual hosting} *)

  val virtual_hosts :
    ?meth:[ `ByURL | `SNI ] -> (string * handler) list -> handler
  (** [virtual_hosts ?meth [(domain, handler); ...]] produces a {!type:handler}
      which enables virtual hosting at the TLS-layer using SNI.
      - [meth] can be used to choose which source to match the hostnames
        against. Defaults to [`SNI]. *)

  (** {1 Logging} *)

  val set_log_lvl : Logs.level -> unit
  (** Set Mehari's logger to the given log level. *)

  val logger : clock -> middleware
  (** Logs and times requests. Time spent logging is included. *)

  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

(** Module type containing all file system dependent functions. *)
module type FS = sig
  module IO : Signatures.IO

  type handler = request -> response IO.t
  type dir_path

  (** {1 Static files} *)

  val respond_document : ?mime:mime -> dir_path -> response IO.t
  (** Same as {!val:Mehari.Response.respond} but respond with content of given
      [filename] and use given {!type:Mehari.mime} as mime type. If [filename]
      is not present on filesystem, responds with
      {!val:Mehari.Response.Status.not_found}. If [mime] parameter is not
      supplied, document is served as {!val:Mehari.Mime.app_octet_stream}. *)

  val static :
    ?handler:(dir_path -> handler) ->
    ?dir_listing:
      (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    dir_path ->
    handler
  (** [static dir] validates the path parameter (retrieved by calling
      [Mehari.Request.param req 1]) by checking that it is relative and does not
      contain parent directory references. If these checks fail, responds with
      {!val:Mehari.Response.Status.not_found}.

      If the checks succeed, [static] calls [handler path request], where [path]
      is the path generated by the concatenation of directory that was passed to
      [static] and path of request. [handler] defaults to
      {!val:respond_document}.

      If a directory is requested, [static] will look for a file named [index]
      in that directory to return. Otherwise, a directory file listing will be
      generated by calling [dir_listing [ filename; ... ] request]. [index] is
      default on [index.gmi].

      Mime type of the served ressource is guessed by checking file name. Note
      that file names of the form [*.gmi] will be served as [text/gemini].

      [show_hidden] decides whether hidden files should be listed. It defaults
      to [false] for security reasons. *)
end

(**/**)

(** {1 Private} *)

(** You can ignore it, unless you are porting [Mehari] to a new platform not
    supported by the existing IO backends. *)
module Private : sig
  module type IO = Signatures.IO
  module type PCLOCK = Signatures.PCLOCK

  module Cert : sig
    val get_certs :
      exn_msg:string -> Tls.Config.certchain list -> Tls.Config.own_cert
  end

  module Cgi = Cgi

  module Logger_impl : sig
    module type S = sig
      module IO : IO

      type handler = request -> response IO.t
      type clock

      val set_level : Logs.level -> unit
      val logger : clock -> handler -> handler
      val debug : 'a Logs.log
      val info : 'a Logs.log
      val warning : 'a Logs.log
      val error : 'a Logs.log
    end

    module Make
        (Clock : Signatures.PCLOCK)
        (IO : sig
          include IO

          val finally : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
        end) : S with module IO = IO and type clock = Clock.t
  end

  module Protocol = Protocol
  module Rate_limiter_impl = Rate_limiter_impl

  module Router_impl : sig
    module type S = sig
      module IO : IO

      type route
      type rate_limiter
      type handler = request -> response IO.t
      type middleware = handler -> handler

      val no_middleware : middleware
      val pipeline : middleware list -> middleware
      val router : route list -> handler

      val route :
        ?rate_limit:rate_limiter ->
        ?mw:middleware ->
        ?regex:bool ->
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
      S with module IO = RateLimiter.IO and type rate_limiter := RateLimiter.t
  end

  module Static : sig
    module type DIR = sig
      module IO : Signatures.IO

      type path

      val exists : path -> bool IO.t
      val kind : path -> [ `Regular_file | `Directory | `Other ] IO.t
      val read : path -> string list IO.t
      val concat : path -> string -> path
      val respond_document : ?mime:mime -> path -> response IO.t
      val pp_io_err : Format.formatter -> exn -> unit
    end

    module type S = sig
      module IO : Signatures.IO

      type handler = request -> response IO.t
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

    module Make (Dir : DIR) :
      S with module IO := Dir.IO and type dir_path := Dir.path
  end
end

(**/**)
