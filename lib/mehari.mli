(** {Types} *)

type request
type response
type handler = request -> response Lwt.t
type route
type 'a status
type mime
type body

(** {Gemtext}  *)

module Gemtext : sig
  type t = line list

  and line =
    | Text of string
    | Link of { url : string; name : string option }
    | Preformat of preformat
    | Heading of [ `H1 | `H2 | `H3 ] * string
    | ListItem of string
    | Quote of string

  and preformat = { alt : string option; text : string }

  val text : string -> line
  val link : ?name:string -> string -> line
  val preformat : ?alt:string -> string -> line
  val heading : [ `H1 | `H2 | `H3 ] -> string -> line
  val list_item : string -> line
  val quote : string -> line
end

(** {Request} **)

val uri : request -> Uri.t
val addr : request -> Unix.inet_addr
val port : request -> int

(** {Response} *)

val response : 'a status -> 'a -> response
val respond : 'a status -> 'a -> response Lwt.t
val respond_text : string -> response Lwt.t
val respond_gemtext : Gemtext.t -> response Lwt.t
val respond_document : ?mime:mime -> string -> response Lwt.t

(** {Status} *)

val input : string status
val sensitive_input : string status
val success : body -> mime status
val redirect_temp : string status
val redirect_permanent : string status
val temporary_failure : string status
val server_unavailable : string status
val cgi_error : string status
val proxy_error : string status
val slow_down : int -> string status
val permanent_failure : string status
val not_found : string status
val gone : string status
val proxy_request_refused : string status
val bad_request : string status
val client_certificate_required : string status
val certificate_not_authorised : string status
val certificate_not_valid : string status

(** {Body} *)

val text : string -> body
val gemtext : Gemtext.t -> body
val lines : string list -> body
val page : title:string -> string -> body

(** {Mime} *)

val make_mime :
  ?charset:string -> ?lang:string list -> ?mime:string -> unit -> mime

val from_filename : ?charset:string -> ?lang:string list -> string -> mime
val empty : mime
val gemini : mime
val text_mime : string -> mime
val with_charset : mime -> string -> mime
val with_lang : mime -> string list -> mime
val with_mime : mime -> string -> mime

(** {Routing} *)

val router : route list -> handler
val route : string -> handler -> route
val scope : string -> route list -> route

(** {Entry point} *)

val serve :
  ?port:int -> ?cert_file:string -> ?key_file:string -> handler -> 'a Lwt.t

val run : ?port:int -> ?cert_file:string -> ?key_file:string -> handler -> unit
