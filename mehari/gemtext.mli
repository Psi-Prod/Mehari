(** Implementation of
    {{:https://geminiprotocol.net/docs/gemtext-specification.gmi}Gemtext}, the
    Gemini own native response format.

    Note that if a string containing line breaks ([CR] or [CRLF]) is given to
    functions {!val:heading}, {!val:list_item} and {!val:quote} only the first
    line will be formatted and the others treated as normal text. To avoid this
    behavior, see {!val:Mehari.paragraph}.

    {@ocaml[
      open Mehari.Gemtext

      let () =
        assert ([ quote "hello\nworld" ] = [ quote "hello"; text "world" ])
    ]} *)

type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | ListItem of string
  | Quote of string

and preformat = { alt : string option; text : string }

val line_to_string : line -> string
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
val pp_line : Format.formatter -> line -> unit
val pp : Format.formatter -> t -> unit
