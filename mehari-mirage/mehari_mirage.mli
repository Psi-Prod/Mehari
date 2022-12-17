(** Mirage OS compatiblity. *)

module type S = sig
  module IO = Lwt
  include Mehari.NET with module IO := IO

  include Server_impl.S with module IO := IO

  (** {1 Entry point} *)

  val run :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t
  (** [run ?port ?certchains stack handler] runs the server using
      [handler].
        - [port] is the port to listen on. Defaults to [1965].
        - [certchains] is the list of form [[(cert_path, privatekey_path); ...]],
          the last one is considered default.

      @raise Invalid_argument if [certchains] is empty. *)
end

(** A functor building an IO module from Mirage components. *)
module Make (Clock : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) :
  S with type stack = Stack.t
