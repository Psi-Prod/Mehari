(** Logger implementation. *)

val src : Logs.src
(** Mehari's logs source. *)

val logger : Handler.t -> Handler.t
(** Logs and times requests. Time spent logging is included. *)
