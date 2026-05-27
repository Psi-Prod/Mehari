(** File system features implementation. *)

open Mehari

type path = string

val respond_document : ?mime:Mime.t -> path -> Response.t

val static :
  ?handler:(path -> handler) ->
  ?dir_listing:
    (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
  ?index:string ->
  ?show_hidden:bool ->
  path ->
  string ->
  handler
