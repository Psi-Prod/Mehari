(** Gemini server implementation. *)

(** {1 Runtime} *)

module Runtime = Runtime
(** Runtime required by a Gemini server.*)

(** {1 Server} *)

module type S = sig
  type stack
  (** Represents a network stack.*)

  val run :
    ?timeout:float ->
    ?port:int ->
    certs:Tls.Config.certchain list ->
    stack ->
    Handler.t ->
    unit
  (** [run ?port ?timeout ~certs stack handler] runs a Gemini server.

      - [port] is the port to listen on. Defaults to [1965].
      - [timeout] is the maximum time in seconds to wait for client to write a
        request after the TLS handshake. Unset by default.
      - [certs] is the TLS certchains used. See section "Certificate selection"
        bellow.

      @raise Invalid_argument msg on invalid TLS config.

      {2 Certificate selection}

      If the server is configured with only a single certificate, it is always
      used.

      If the client requests a specific server name:
      - find a strict match
      - find a wildcard match
      - use the default one if present. *)
end

module Make : (R : Runtime.S) -> S with type stack := R.stack
