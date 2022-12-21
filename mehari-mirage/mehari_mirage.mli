(** Mirage OS compatiblity. *)

module type S = sig
  module IO = Lwt

  type stack
  (** Tcpip stack. *)

  (** {1 Net} *)

  (** @closed *)
  include Mehari.NET with module IO := IO and type addr = Ipaddr.t

  (** {1:response Response} *)

  val respond : 'a Mehari.status -> 'a -> Mehari.response IO.t
  (** Same as {!val:Mehari.response}, but the new {!type:Mehari.response} is
        wrapped in a promise. *)

  val respond_body : Mehari.body -> Mehari.mime -> Mehari.response IO.t
  (** Same as {!val:respond} but respond with given {!type:Mehari.body}
        and use given {!type:Mehari.mime} as mime type. *)

  val respond_text : string -> Mehari.response IO.t
  (** Same as {!val:respond} but respond with given text and use [text/plain] as
        {!type:Mehari.mime} type. *)

  val respond_gemtext : Mehari.Gemtext.t -> Mehari.response IO.t
  (** Same as {!val:respond} but respond with given {!type:Mehari.Gemtext.t} and use
        [text/gemini] as {!type:Mehari.mime} type. *)

  val respond_raw :
    [ `Body of string | `Full of int * string * string ] -> Mehari.response IO.t
  (** Same as {!val:Mehari.response_raw}, but the new {!type:Mehari.response}
        is wrapped in a promise. *)

  (** {1 Entry point} *)

  val run_lwt :
    ?port:int ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t
  (** [run ?port ?certchains stack handler] runs the server using
      [handler].
        - [port] is the port to listen on. Defaults to [1965].
        - [certchains] is the list of form [[(cert_path, private_key_path); ...]],
          the last one is considered default.

      @raise Invalid_argument if [certchains] is empty. *)

  val run :
    ?port:int ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit
  (** Like {!val:run_lwt} but calls [Lwt_main.run] internally. *)
end

(** A functor building an IO module from Mirage components. *)
module Make (Clock : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) :
  S with type stack = Stack.t
