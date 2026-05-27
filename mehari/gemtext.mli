(** Implementation of
    {{:https://geminiprotocol.net/docs/gemtext-specification.gmi}Gemtext}, the
    Gemini own native response format.

    Note that if a string containing line breaks ([CR] or [CRLF]) is given to
    functions {!val:heading}, {!val:list_item} and {!val:quote} only the first
    line will be formatted and the others treated as normal text. To avoid this
    behavior, see {!val:paragraph}.

    {@ocaml[
    open Mehari.Gemtext

    let () = assert ([ quote "hello\nworld" ] = [ quote "hello"; text "world" ])
    ]} *)

type t = line list

and line =
  | Text of string
  | Link of { url : string; name : string option }
  | Preformat of preformat
  | Heading of [ `H1 | `H2 | `H3 ] * string
  | List_item of string
  | Quote of string

and preformat = { alt : string option; text : string }

val line_to_string : line -> string
(** Convert a gemtext line to a string. *)

val of_string : string -> t
(** Creates a gemtext document from given string. *)

val to_string : t -> string
(** Convert a gemtext document to a string. *)

val paragraph : (string -> line) -> string -> t
(** [paragraph to_gemtext str] is a convenient function to transform a string
    containing line breaks ([CR] or [CRLF]) into a Gemtext document.

    {@ocaml[
    open Mehari.Gemtext

    let () =
      assert (paragraph quote "hello\nworld" = [ quote "hello"; quote "world" ])
    ]} *)

(** {1 Smart constructors} *)

val text : string -> line

val newline : line
(** [newline] is [text ""]. *)

val link : ?name:string -> string -> line
val preformat : ?alt:string -> string -> line
val heading : [ `H1 | `H2 | `H3 ] -> string -> line
val list_item : string -> line
val quote : string -> line

(** {1 Pretty printing} *)

val pp_line : Format.formatter -> line -> unit
val pp : Format.formatter -> t -> unit

(** {1 Equality} *)

val equal_line : line -> line -> bool
val equal : t -> t -> bool
