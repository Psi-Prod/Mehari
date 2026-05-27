(** Rate limiter implementation. *)

type t
(** Representing a rate limiter to attach to a route. *)

val make : ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
(** [make ~period n unit] creates a rate limiter which limits client to [n]
    request per [period * unit]. For example,
    {[
    make ~period:2 5 `Hour
    ]}
    limits client to 5 requests every 2 hours. *)

val check : t -> Request.t -> Response.t option
