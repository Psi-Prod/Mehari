(** An IO module Mehari implementation based on [Eio] library. *)

open Mehari

(** {1 Net} *)

(** @closed *)
include
  NET
    with type clock := [ `Clock of float ] Eio.Time.clock
     and module IO := Identity_reader_monad

(** @closed *)
include
  FS
    with type dir_path := [ `Dir ] Eio.Path.t
     and module IO := Identity_reader_monad

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
include SERVER with type config := config and module IO := Identity_reader_monad
