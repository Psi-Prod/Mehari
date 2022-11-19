(** Mehari implementation for Unix and Windows using Lwt. *)

include Mehari.IO

val run_lwt :
  ?port:int ->
  ?certchains:(string * string) list ->
  Mehari.handler ->
  unit Lwt.t
(** See {!val:Mehari.IO.run_lwt}. *)

val run :
  ?port:int -> ?certchains:(string * string) list -> Mehari.handler -> unit
(** Like {!val:Mehari.IO.run_lwt} but calls [Lwt_main.run] internally. *)
