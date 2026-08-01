(** Runtime required by a Gemini server. *)

type tls_error =
  [ `Tls_alert of Tls.Packet.alert_type | `Tls_failure of Tls.Engine.failure ]

module type S = sig
  (** {1 Time} *)

  val sleep : float -> unit
  (** [sleep duration] slept during [duration] seconds. *)

  val now : unit -> Ptime.t
  (** [now ()] returns the current POSIX time. *)

  (** {1 Net} *)

  type stack
  (** Represents the type of a TCP net stack. *)

  type listener
  (** Represents the type of TCP socket to listen on. *)

  val listen : stack -> int -> listener
  (** [listen stack port] prepares [port] for receiving incoming TCP connection
      requests. *)

  module TLS : sig
    type t
    (** Represents the type of a TLS session. *)

    val peer : t -> Ipaddr.t * int
    (** [peers flow] returns a pair representing the client address and its port
        number. *)

    val epoch : t -> Tls.Core.epoch_data option
    (** [epoch t] returns [epoch], which contains information of the active
        session. Returns [None] if the current session has been closed b the
        client. *)

    val really_read :
      t -> ?off:int -> ?len:int -> bytes -> (unit, [> tls_error ]) result
    (** [really_read flow buf ~off ~len] tries to read [len] bytes (defaults to
        [Bytes.length buf - off]) from the given TLS {i socket} [flow], storing
        them in byte sequence [buf], starting at position [off] in [buf]
        (defaults to [0]). If [len = 0], [really_read] does nothing.

        @raise End_of_file
          if read returns [0] before [len] characters have been read.

        @raise Invalid_argument
          if [off] and [len] do not designate a valid range of [buf]. *)

    val write :
      t ->
      ?off:int ->
      ?len:int ->
      string ->
      (unit, [> `Connection_closed ]) result
    (** [write flow str ~off ~len] tries to write [len] bytes (defaults to
        [String.length str - off]) from byte sequence [str], starting at offset
        [off] (defaults to [0]), to the given TLS {i socket} [flow].

        Fails if [flow] is connected to a peer whose reading end is closed.

        @raise Invalid_argument
          if [off] and [len] do not designate a valid range of [buf]. *)

    val close : t -> unit
    (** [close flow] closes the TLS session. *)
  end

  module TCP : sig
    type t
    (** An individual outgoing TCP connection. *)

    val accept : stack -> listener -> t
    (** [accept state listen] blocks until a client connects to the port
        associated with [listen], then returns a {!type:t} connected to that
        client. *)

    val tls_upgrade : Tls.Config.server -> t -> TLS.t
    (** [tls_upgrade tls_config flow] upgrades the given TCP flow to TLS using
        the given server configuration.

        @raise End_of_file if we are not able to complete the handshake. *)
  end
end
