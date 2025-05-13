(** X509 certificates and TLS certchains related types. *)

(** Type for certchains providing. *)
type t =
  | Single of certchain
  | Multiple of certchain list
  | Multiple_default of certchain * certchain list

and certchain = Tls.Config.certchain

(**/**)

module Private : sig
  val to_own_cert : t -> Tls.Config.own_cert
  (** Constructs a value of type {!Tls.Config.own_cert} from a {!t}. *)

  val supports_hostname : t -> [ `host ] Domain_name.t -> bool
  (** [supports_hostname certs h] checks if one certifcates contains in [certs]
      supports the hostname [h]. *)
end

(**/**)
