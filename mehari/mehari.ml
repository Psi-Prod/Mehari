type 'addr request = 'addr Request.t
type response = Response.t
type 'a status = 'a Response.status
type mime = Mime.t
type body = Response.body

module Gemtext = Gemtext

let paragraph = Gemtext.paragraph
let uri = Request.uri
let target = Request.target
let ip = Request.ip
let port = Request.port
let sni = Request.sni
let query = Request.query
let client_cert = Request.client_cert
let param = Request.param
let response = Response.response
let response_body = Response.response_body
let response_text = Response.response_text
let response_gemtext = Response.response_gemtext
let response_raw = Response.response_raw

include Response.Status

let string = Response.string
let gemtext = Response.gemtext
let lines = Response.lines
let seq = Response.seq
let stream = Response.stream
let page = Response.page
let make_mime = Mime.make_mime
let from_filename = Mime.from_filename
let from_content = Mime.from_content
let no_mime = Mime.no_mime
let gemini = Mime.gemini
let app_octet_stream = Mime.app_octet_stream
let plaintext = Mime.plaintext
let text = Mime.text
let with_charset = Mime.with_charset

module type NET = sig
  module IO : Types.IO

  type route
  type rate_limiter
  type addr
  type handler = addr Request.t -> Response.t IO.t
  type middleware = handler -> handler
  type clock

  val no_middleware : middleware
  val pipeline : middleware list -> middleware
  val router : route list -> handler

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?regex:bool ->
    string ->
    handler ->
    route

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route

  val no_route : route

  val make_rate_limit :
    clock ->
    ?period:int ->
    int ->
    [ `Second | `Minute | `Hour | `Day ] ->
    rate_limiter

  val virtual_hosts :
    ?meth:[ `ByURL | `SNI ] -> (string * handler) list -> handler

  val set_log_lvl : Logs.level -> unit
  val logger : clock -> handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

module type FS = sig
  module IO : Types.IO

  type addr
  type handler = addr Handler.Make(IO).t
  type dir_path

  val response_document : ?mime:mime -> dir_path -> response IO.t

  val static :
    ?handler:(dir_path -> handler) ->
    ?dir_listing:
      (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    dir_path ->
    handler
end

module Private = struct
  module type IO = Types.IO
  module type ADDR = Types.ADDR
  module type PCLOCK = Types.PCLOCK

  type response_view = Response.view

  let view_of_resp = Response.view_of_resp

  module Cert = struct
    let get_certs ~exn_msg = function
      | default :: mult -> `Multiple_default (default, mult)
      | _ -> invalid_arg exn_msg
  end

  module CGI = Cgi
  module Handler = Handler
  module Logger_impl = Logger_impl
  module Protocol = Protocol
  module Rate_limiter_impl = Rate_limiter_impl
  module Router_impl = Router_impl
  module Static = Static
end
