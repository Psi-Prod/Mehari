module Addr = Common.Addr
module Direct = Common.Direct
module IO = Common.Direct

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
type stack

include Mehari.Private.MakeResponse (Direct)

let make_rate_limit = RateLimiter.make
let set_log_lvl = Logger.set_level
let logger = Logger.logger
let debug = Logger.debug
let info = Logger.info
let warning = Logger.warning
let error = Logger.error
let router = Router.router
let route = Router.route
let scope = Router.scope
let run = Server.run