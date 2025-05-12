(** MIME type handling. *)

type t
(** Mime type of a document. *)

val make : ?charset:string -> string -> t
(** [make ?charset mime] creates a {!type:t} type from given [charset]. Charset
    defaults to [utf-8] if [mime] begins with [text/].

    @raise Invalid_argument if [mime] is an empty string

    @see < https://www.rfc-editor.org/rfc/rfc2046#section-4.1.2 >
      For a description of the "charset" parameter. *)

val from_filename : ?charset:string -> string -> t option
(** [from_filename ?charset fname] tries to create a MIME by performing a MIME
    lookup based on file extension of [fname].

    Note that MIME {!val:gemini} are not infered from files with [.gmi]
    extension. See {:https://github.com/Psi-Prod/Mehari/issues/36}. *)

val from_content : ?charset:string -> tree:Conan.Tree.t -> string -> t option
(** [from_content ?charset ~tree c] tries to create a MIME type by performing a
    MIME lookup based on content [c]. [tree] is the tree used to build the MIME
    database. *)

val with_charset : string -> t -> t
(** Set charset of given {!type:t}. *)

val gemini : ?charset:string -> ?lang:string list -> unit -> t
(** [gemini ?charset ?lang ()] is [text/gemini; charset=...; lang=...].

    @see < https://www.rfc-editor.org/rfc/rfc2046#section-4.1.2 >
      For a description of the "charset" parameter.

    @see < https://www.ietf.org/rfc/bcp/bcp47.txt >
      For a description of the "lang" parameter. *)

val app_octet_stream : t
(** [app_octet_stream] is a shortcut for [application/octet-stream]. *)

val plaintext : t
(** [plaintext] is a shortcut for [text/plain; charset=utf-8]. *)

val text : string -> t
(** [text "type"] is a shortcut for [text/type; charset=utf-8]. *)

(** {1 Utils} *)

val equal : t -> t -> bool
(** Equality between MIME type. *)

val to_string : t -> string
(** [to_sting m] is the string representation of [m]. *)
