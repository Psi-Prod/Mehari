(** Implementation of the Gemini own native response format. *)

type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

val of_string : string -> t
val to_string : t -> string

(** {1 Facilities} *)

val text : string -> line

val newline : line
(** [newline] is [text ""]. *)

val link : ?name:string -> string -> line
val preformat : ?alt:string -> string -> line
val heading : [ `H1 | `H2 | `H3 ] -> string -> line
val list_item : string -> line
val quote : string -> line
