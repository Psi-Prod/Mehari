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

module type NET = Signatures.NET
module type FS = Signatures.FS

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

    module Make (RateLimiter : Rate_limiter_impl.S) (_ : Logger_impl.S) :
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
