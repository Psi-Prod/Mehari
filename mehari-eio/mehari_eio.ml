module Addr = Common.Addr
module Direct = Common.Direct
module IO = Common.Direct

module type S =
  Mehari.NET with module IO := Direct and type addr = Eio.Net.Ipaddr.v4v6

(** TODO: replace Pclock by mirage-clock-eio when it becomes available. *)

module RateLimiter =
  Mehari.Private.Rate_limiter_impl.Make (Pclock) (Direct) (Addr)

module Logger =
  Mehari.Private.Logger_impl.Make
    (Pclock)
    (struct
      include Direct

      let finally t f r = try f (t ()) with exn -> r exn
    end)
    (Addr)

module Router = Mehari.Private.Router_impl.Make (RateLimiter) (Logger)
module Server = Server_impl.Make (Logger)

type addr = Addr.t
type handler = Router.handler
type middleware = handler -> handler
type route = Router.route
type rate_limiter = RateLimiter.t

let set_log_lvl = Logger.set_level
let logger = Logger.logger
let debug = Logger.debug
let info = Logger.info
let warning = Logger.warning
let error = Logger.error
let no_middleware = Router.no_middleware
let pipeline = Router.pipeline
let router = Router.router
let route = Router.route
let scope = Router.scope
let no_route = Router.no_route
let virtual_hosts = Router.virtual_hosts
let make_rate_limit = RateLimiter.make
let response_document = File.response_document
let static = File.static
let run = Server.run
