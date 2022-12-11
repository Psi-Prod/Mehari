type request = Request.t
type response = Response.t
type handler = Handler.t
type 'a status = 'a Response.status
type mime = Mime.t
type body = Response.body

let uri = Request.uri
let ip = Request.ip
let port = Request.port
let sni = Request.sni
let query = Request.query
let param = Request.param

include Response.Status
module Gemtext = Gemtext

let text = Response.text
let gemtext = Response.gemtext
let lines = Response.lines
let stream = Response.stream
let page = Response.page

include Mime

let make_mime = Mime.make
let response = Response.response
let respond = Response.respond
let respond_body = Response.respond_body
let respond_text = Response.respond_text
let respond_gemtext = Response.respond_gemtext

module type IO = sig
  type middleware = handler -> handler
  type route
  type stack
  type rate_limiter

  val make_rate_limit :
    ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> rate_limiter

  val init_log : Logs.level -> unit
  val logger : Handler.t -> Handler.t
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log

  include
    Router_impl.S with type t := route and type rate_limiter := rate_limiter

  val run_lwt :
    ?port:int ->
    ?certchains:(string * string) list ->
    stack ->
    Handler.t ->
    unit Lwt.t
end

module Mirage = struct
  module Make (Clock : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) :
    IO with type stack = Stack.t = struct
    module RateLimiter = Rate_limiter_impl.Make (Clock)
    module Router = Router_impl.Make (RateLimiter)
    module Logger = Logger_impl.Make (Clock)
    module Server = Server_impl.Make (Stack)

    type middleware = handler -> handler
    type route = Router.t
    type rate_limiter = RateLimiter.t
    type stack = Stack.t

    let init_log = Logger.init

    include Logger

    let logger = Logger.logger
    let router = Router.router
    let route = Router.route
    let scope = Router.scope
    let make_rate_limit = RateLimiter.make
    let run_lwt = Server.run
  end
end
