(** TLS certchains and X509 certificates related types. *)

(** Type for certchains providing. *)
type t =
  | Single of certchain
  | Multiple of certchain list
  | Multiple_default of certchain * certchain list

and certchain = Tls.Config.certchain

(**/**)

module Private : sig
  val make_config : t -> Tls.Config.server
  (** Build a server configuration from given certchains. *)
end

(**/**)
