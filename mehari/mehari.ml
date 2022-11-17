module type S = sig
  type request
  type response
  type handler = request -> response Lwt.t
  type route
  type 'a status
  type mime
  type body
  type middleware = handler -> handler
  type rate_limiter

  module Gemtext : sig
    type t = line list

    and line =
      | Text of string
      | Link of { url : string; name : string option }
      | Preformat of { alt : string option; text : string }
      | Heading of [ `H1 | `H2 | `H3 ] * string
      | ListItem of string
      | Quote of string

    val text : string -> line
    val link : ?name:string -> string -> line
    val preformat : ?alt:string -> string -> line
    val heading : [ `H1 | `H2 | `H3 ] -> string -> line
    val list_item : string -> line
    val quote : string -> line
  end

  val uri : request -> Uri.t
  val addr : request -> Unix.inet_addr
  val port : request -> int
  val sni : request -> string option
  val response : 'a status -> 'a -> response
  val respond : 'a status -> 'a -> response Lwt.t
  val respond_body : body -> mime -> response Lwt.t
  val respond_text : string -> response Lwt.t
  val respond_gemtext : Gemtext.t -> response Lwt.t
  val respond_document : ?mime:mime -> string -> response Lwt.t
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
  val text : string -> body
  val gemtext : Gemtext.t -> body
  val lines : string list -> body
  val page : title:string -> string -> body
  val make_mime : ?charset:string -> ?lang:string list -> string -> mime
  val from_filename : ?charset:string -> ?lang:string list -> string -> mime
  val empty : mime
  val gemini : mime
  val text_mime : string -> mime
  val with_charset : mime -> string -> mime
  val with_lang : mime -> string list -> mime
  val with_mime : mime -> string -> mime
  val router : route list -> handler

  val route :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> handler -> route

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route

  val make_rate_limit :
    ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> rate_limiter

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
end

module Make
    (Clock : Mirage_clock.PCLOCK)
    (KV : Mirage_kv.RO)
    (Stack : Tcpip.Stack.V4V6) : S = struct
  module RateLimiter = Rate_limiter_impl.Make (Clock)
  module Router = Router_impl.Make (RateLimiter)
  module Server = Server_impl.Make (Clock) (KV) (Stack)

  type request = Request.t
  type response = Response.t
  type handler = Handler.t
  type route = Router.t
  type 'a status = 'a Response.status
  type mime = Mime.t
  type body = Response.body
  type middleware = handler -> handler
  type rate_limiter = RateLimiter.t

  let uri = Request.uri
  let addr = Request.addr
  let port = Request.port
  let sni = Request.sni

  include Response.Status
  module Gemtext = Gemtext

  let text = Response.text
  let gemtext = Response.gemtext
  let lines = Response.lines
  let page = Response.page

  include Mime

  let make_mime = Mime.make
  let response = Response.response
  let respond = Response.respond
  let respond_body = Response.respond_body
  let respond_text = Response.respond_text
  let respond_gemtext = Response.respond_gemtext
  let respond_document = Response.respond_document
  let router = Router.router
  let route = Router.route
  let scope = Router.scope
  let make_rate_limit = RateLimiter.make
  let run = Server.run
  let run_lwt = Server.run_lwt
end
