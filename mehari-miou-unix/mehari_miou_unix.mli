(** An IO module Mehari implementation for Unix and Windows using Miou. *)

open Mehari

(** {1 Net} *)

(** @closed *)
include NET with type clock := unit and module IO := Identity_monad

(** {1 Rate limit} *)

val make_rate_limit :
  ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> rate_limiter
(** Same as {!val:Mehari.NET.make_rate_limit} but without the required trailing
    unit parameter. *)

(** {1 Logging} *)

val logger : handler -> handler
(** Same as {!val:Mehari.NET.logger} but without the required trailing unit
    parameter. *)

(** {1 Run server} *)

(** Server configuration. *)
module Config : sig
  type t

  val make :
    ?reuseport:bool ->
    ?reuseaddr:bool ->
    ?backlog:int ->
    ?ip:Ipaddr.Prefix.t ->
    unit ->
    t
  (** Build a configuration using given parameters.

      - [ip] is the server IP addresses. Defaults to [127.0.0.1/8].
      - [backlog] is the the number of pending connections that can be queued
        up. Defaults to [64].
      - [reuseaddr] allows reuse of local addresses for bind. Defaults to true.
      - [reuseport] allows reuse of address and port bindings. Defaults to true.
  *)
end

(** @inline *)
include SERVER with type config := Config.t and module IO := Identity_monad
