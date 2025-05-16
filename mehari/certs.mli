(** TLS certchains related types. *)

(** Type for TLS certchains providing. *)
type t =
  | Single of certchain
  | Multiple of certchain list
  | Multiple_default of certchain * certchain list

and certchain = Tls.Config.certchain

(** {1 Certificate selection}

    If the server is configured with only a single certificate, it is always
    used.

    If the client does not request for a server name using SNI, the default
    certificate is used, if present.

    If the client requests a specific server name:
    - find a strict match
    - find a wildcard match
    - use the default one if present. *)

(**/**)

module Private : sig
  val make_config : t -> Tls.Config.server
  (** Build a server configuration from given certchains. *)
end

(**/**)
