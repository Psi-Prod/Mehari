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

(** {1 TLS certificates} *)

module Certs = Certs

(** {1 IO} *)

module type NET = Signatures.NET
module type FS = Signatures.FS
module type SERVER = Signatures.SERVER

(**/**)

(** {1 Private} *)

(** You can ignore it, unless you are porting [Mehari] to a new platform not
    supported by the existing IO backends. *)
module Private : sig
  module type IO = Signatures.IO
  module type PCLOCK = Signatures.PCLOCK

  module Cgi = Cgi
  module Logger_impl = Logger_impl
  module Protocol = Protocol
  module Rate_limiter_impl = Rate_limiter_impl
  module Router_impl = Router_impl
  module Signatures = Signatures
  module Static = Static
end

(**/**)
