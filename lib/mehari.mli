(** {1 Types} *)

type request
type response
type handler = request -> response Lwt.t
type 'a status
type mime
type body
type gemtext = Gemtext.t

(** {2 Request} **)

val uri : request -> Uri.t
val addr : request -> Unix.inet_addr
val port : request -> int

(** {3 Body} *)

val text : string -> body
val gemtext : Gemtext.t -> body

(** {4 Response} *)

val response : 'a status -> 'a -> response
val respond : 'a status -> 'a -> response Lwt.t
val make_mime : ?charset:string -> ?lang:string -> ?mime:string -> unit -> mime
val empty_mime : mime
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

(** {5 Entry point} *)

val serve :
  ?port:int -> ?cert_file:string -> ?key_file:string -> handler -> 'a Lwt.t

val run : ?port:int -> ?cert_file:string -> ?key_file:string -> handler -> unit
