(** An IO module Mehari implementation for Unix and Windows using Lwt. Contains
    also extra features based on Unix filesystem such as {!section-cgi}. *)

open Mehari

(** {1 Net} *)

include Mehari_mirage.S with type stack := unit
(** @closed *)

(** {1 Filesystem} *)

(** @closed *)
include FS with type dir_path := string and module IO := Lwt

(** {1:cgi CGI} *)

(** @closed *)
include CGI with module IO := Lwt

(** {1 Run server} *)

(** Server configuration. *)
module Config : sig
  type t

  (** IP configuration. *)
  type ip =
    | IPv4 of Ipaddr.V4.Prefix.t
    | IPv6 of Ipaddr.V6.Prefix.t
    | IPv4v6 of Ipaddr.V4.Prefix.t * Ipaddr.V6.Prefix.t

  val make : ip -> t
  (** Build a configuration using given parameters. *)

  val default : t
  (** The default configuration listen on [127.0.0.1/8]. *)
end

(** @inline *)
include SERVER with type config := Config.t and module IO := Lwt
