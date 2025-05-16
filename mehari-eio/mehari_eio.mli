(** An IO module Mehari implementation based on [Eio] library. *)

open Mehari

(** {1 Net} *)

module IO = Identity_reader_monad

(** @closed *)
include
  NET with module IO := IO and type clock := [ `Clock of float ] Eio.Time.clock

(** @closed *)
include FS with module IO := IO and type dir_path := [ `Dir ] Eio.Path.t

(** {1 Server configuration} *)

type config
(** Configuration parameters *)

val config : ?backlog:int -> ?addr:Eio.Net.Ipaddr.v4v6 -> unit -> config
(** Build a {!type-config} from given parameters.

    - [backlog] is the the number of pending connections that can be queued up.
      Defaults to [4096].
    - [addr] is the socket addresses. Defaults to [Eio.Net.Ipaddr.V4.loopback].
*)

(** {1 Run server} *)

(** @inline *)
include SERVER with type config := config and module IO := IO
