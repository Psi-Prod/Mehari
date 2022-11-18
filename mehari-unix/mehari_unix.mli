(** Mehari implementation for Unix and Windows using Lwt. *)

include Mehari.S

val run :
  ?port:int ->
  ?addr:string ->
  ?certchains:(string * string) list ->
  handler ->
  unit

val run_lwt :
  ?port:int ->
  ?addr:string ->
  ?certchains:(string * string) list ->
  handler ->
  'a Lwt.t
