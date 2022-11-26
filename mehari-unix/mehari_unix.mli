(** An IO module Mehari implementation for Unix and Windows. Contains also
    extra features based on Unix. *)

include Mehari.IO

(** {1 Response} *)

val respond_document : ?mime:Mehari.mime -> string -> Mehari.response Lwt.t
(** Same as {!val:Mehari.respond} but respond with content of given [filename]
    and use given {!type:Mehari.mime} as mime type.
    If [filename] is not present on filesystem, responds with
    {!val:Mehari.not_found}. {!type:Mehari.mime} type is chosen according to
    the filename extension by default. If mime type inference failed, it uses
    [text/gemini; charset=utf-8]. *)

include Mehari.IO

(** {1 Response} *)

val respond_document : ?mime:Mehari.mime -> string -> Mehari.response Lwt.t
(** Same as {!val:Mehari.respond} but respond with content of given [filename]
    and use given {!type:Mehari.mime} as mime type.
    If [filename] is not present on filesystem, responds with
    {!val:Mehari.not_found}. {!type:Mehari.mime} type is chosen according to
    the filename extension by default. If mime type inference failed, it uses
    [text/gemini; charset=utf-8]. *)

(** {1 Mime} *)

val from_filename :
  ?lookup:[ `Ext | `Content | `Both ] ->
  ?charset:string ->
  ?lang:string list ->
  string ->
  Mehari.mime Lwt.t
(** [from_filename ?lookup_into ?charset ?lang fname] creates a
    {!type:Mehari.mime} type by performing a mime lookup depending of the value of
    [lookup]:
    - [`Ext]: performs a lookup based on file extension of [fname];
    - [`Content]: performs a lookup based on content of [fname];
    - [`Both]: performs successivly a lookup on content and extension.

    Returns [make_mime ?charset ?lang "text/gemini"] if one of the previous
    lookup fails.

    @raise Unix.Unix_error if a lookup based on content is performed and
      reading of [fname] fails *)

(** {1 Entry point} *)

val run_lwt :
  ?port:int ->
  ?certchains:(string * string) list ->
  ?v4:string ->
  ?v6:string ->
  Mehari.handler ->
  unit Lwt.t
(** See {!val:Mehari.IO.run_lwt}. *)

val run :
  ?port:int ->
  ?certchains:(string * string) list ->
  ?v4:string ->
  ?v6:string ->
  Mehari.handler ->
  unit
(** Like {!val:Mehari.IO.run_lwt} but calls [Lwt_main.run] internally. *)
