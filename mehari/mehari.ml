type request = Request.t
type response = Response.t
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
let response_body = Response.response_body
let response_text = Response.response_text
let response_gemtext = Response.response_gemtext
let response_raw = Response.response_raw

module type IO = sig
  module IO : Io.S
  include Router_impl.S with module IO := IO

  type middleware = handler -> handler
  type stack

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

module Mirage = struct
  module Make (Clock : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) :
    IO with module IO := Lwt and type stack = Stack.t = struct
    module RateLimiter = Rate_limiter_impl.Make (Clock) (Lwt)

    module Logger =
      Logger_impl.Make
        (Clock)
        (struct
          include Lwt

          let finally = try_bind
        end)

    module Router = Router_impl.Make (RateLimiter) (Logger)
    module Server = Server_impl.MirageMake (Stack) (Logger)
    include Logger

    type middleware = handler -> handler
    type route = Router.route
    type rate_limiter = RateLimiter.t
    type stack = Stack.t

    let set_log_lvl = Logger.set_level

    include Response.Make (Lwt)

    let make_rate_limit = RateLimiter.make
    let logger = Logger.logger
    let router = Router.router
    let route = Router.route
    let scope = Router.scope
    let run = Server.run
  end
end
