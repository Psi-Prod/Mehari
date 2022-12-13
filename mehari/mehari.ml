type request = Request.t
type response = Response.t
type 'a status = 'a Response.status
type mime = Mime.t
type body = Response.body

module Gemtext = Gemtext

let uri = Request.uri
let ip = Request.ip
let port = Request.port
let sni = Request.sni
let query = Request.query
let param = Request.param
let response = Response.response
let response_body = Response.response_body
let response_text = Response.response_text
let response_gemtext = Response.response_gemtext
let response_raw = Response.response_raw

include Response.Status

let text = Response.text
let gemtext = Response.gemtext
let lines = Response.lines
let stream = Response.stream
let page = Response.page
let make_mime = Mime.make_mime
let from_filename = Mime.from_filename
let from_content = Mime.from_content
let empty = Mime.empty
let gemini = Mime.gemini
let text_mime = Mime.text_mime
let with_charset = Mime.with_charset

module type NET = sig
  module IO : Io.S

  type route
  type rate_limiter
  type handler = Request.t -> Response.t IO.t
  type middleware = handler -> handler
  type stack

  val router : route list -> handler

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?typ:[ `Raw | `Regex ] ->
    string ->
    handler ->
    route

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route

  include Response.S with module IO := IO

  val make_rate_limit :
    ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> rate_limiter

  val set_log_lvl : Logs.level -> unit
  val logger : handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log

  val run :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t
end

module Private = struct
  module type IO = Io.S

  type response_view = Response.view

  let view_of_resp = Response.view_of_resp
  let make_request = Request.make

  module Handler = Handler
  module Logger_impl = Logger_impl
  module Rate_limiter_impl = Rate_limiter_impl
  module Router_impl = Router_impl
  module MakeResponse = Response.Make
end
