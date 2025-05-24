(** An IO module Mehari implementation based on [Eio] library. *)

open Mehari

(** {1 Net} *)

(** @closed *)
include
  NET
    with type clock := [ `Clock of float ] Eio.Time.clock
     and module IO := Identity_reader_monad

(** {1 Filesystem} *)

(** @closed *)
include
  FS
    with type dir_path := [ `Dir ] Eio.Path.t
     and module IO := Identity_reader_monad

(** {1 CGI} *)

include CGI with module IO := Identity_reader_monad
(** @closed *)

(** {1 Run server} *)

(** Server configuration. *)
module Config : sig
  type t

  val make : ?backlog:int -> ?addr:Eio.Net.Ipaddr.v4v6 -> unit -> t
  (** Build a configuration using given parameters.

      - [backlog] is the the number of pending connections that can be queued
        up. Defaults to [4096].
      - [addr] is the socket addresses. Defaults to
        [Eio.Net.Ipaddr.V4.loopback]. *)
end

(** @inline *)
include
  SERVER with type config := Config.t and module IO := Identity_reader_monad
