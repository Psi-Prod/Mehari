open Mehari.Private

module type S = sig
  module IO = Lwt
  include Mehari.NET with module IO := IO
  include Server_impl.S with module IO := IO
end

module Make (Clock : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) :
  S with type stack = Stack.t = struct
  module IO = Lwt
  module RateLimiter = Rate_limiter_impl.Make (Clock) (IO)

  module Logger =
    Logger_impl.Make
      (Clock)
      (struct
        include Lwt

        let finally = try_bind
      end)

  module Router = Router_impl.Make (RateLimiter) (Logger)
  module Server = Server_impl.Make (Stack) (Logger)

  type handler = Router.handler
  type middleware = handler -> handler
  type route = Router.route
  type rate_limiter = RateLimiter.t
  type stack = Stack.t

  include Mehari.Private.MakeResponse (IO)

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
end
