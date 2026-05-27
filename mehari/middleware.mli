(** Middleware *)

type t = Handler.t -> Handler.t
(** Middlewares take a {!type:Handler.t}, and run some code before or after —
    producing a “bigger” {!type:Handler.t}. See {!section-middleware}. *)

val pipeline : t list -> t
(** Combines a list of middlewares into one, such that these two lines are
    equivalent: [pipeline [ mw1 ; mw2 ] @@ handler] [ mw1 @@ mw2 @@ handler]. *)
