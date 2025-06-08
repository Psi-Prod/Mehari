open Mehari

module type S = sig
  type stack
  (** TCP/IP stack. *)

  (** {1 Net} *)

  (** @closed *)
  include NET with module IO := Lwt and type clock := unit
  (** See {!Mehari.NET}. *)

  (** {1 Rate limit} *)

  val make_rate_limit :
    ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> rate_limiter
  (** Same as {!val:Mehari.NET.make_rate_limit} but without the required
      trailing unit parameter. *)

  (** {1 Logging} *)

  val logger : handler -> handler
  (** Same as {!val:Mehari.NET.logger} but without the required trailing unit
      parameter. *)

  val respond : 'a status -> 'a -> response Lwt.t
  (** Same as {!val:Mehari.Response.respond}, but the new response is wrapped in
      a promise. *)

  val respond_body : body -> mime -> response Lwt.t
  (** Same as {!val:Mehari.Response.body} but respond with given
      {!type:Mehari.body} and use given {!type:Mehari.mime} as mime type. *)

  val respond_text : string -> response Lwt.t
  (** Same as {!val:Mehari.Response.text} but respond with given text and use
      [text/plain] as MIME type. *)

  val respond_gemtext :
    ?charset:string -> ?lang:string list -> Gemtext.t -> response Lwt.t
  (** Same as {!val:Mehari.Response.gemtext} but respond with given
      {!type:Mehari.Gemtext.t} and use [text/gemini] as {!type:Mehari.mime}
      type. *)

  (** {1 Run server} *)

  val log_src : Logs.src
  (** See {!Mehari.SERVER.log_src}. *)

  val run :
    ?port:int ->
    ?timeout:float ->
    certs:Mehari.Certs.t ->
    stack ->
    handler ->
    unit Lwt.t
  (** See {!Mehari.SERVER.run}. *)
end
