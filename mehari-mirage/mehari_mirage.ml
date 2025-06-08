open Mehari

module type S = Interface.S

module Make
    (PClock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S) : Interface.S with type stack = Stack.t = struct
  module IO = Lwt

  module Clock = struct
    type t = unit

    let now_d_ps () = PClock.now_d_ps ()
  end

  module RateLimiter = Private.Rate_limiter_impl.Make (Clock) (IO)

  module Logger =
    Private.Logger_impl.Make
      (Clock)
      (struct
        include Lwt

        let finally = try_bind
      end)

  module Router = Private.Router_impl.Make (RateLimiter) (Logger)
  module Srv = Server.Make (PClock) (Stack) (Time)

  type handler = Router.handler
  type middleware = handler -> handler
  type domain_handler = Router.domain_handler
  type route = Router.route
  type rate_limiter = RateLimiter.t
  type stack = Stack.t

  let respond s i = Response.respond s i |> IO.return
  let respond_body b m = Response.body b m |> IO.return
  let respond_text t = Response.text t |> IO.return

  let respond_gemtext ?charset ?lang g =
    Response.gemtext ?charset ?lang g |> IO.return

  let logger = Logger.logger ()
  let pipeline = Router.pipeline
  let router = Router.router
  let route = Router.route
  let domain = Router.domain
  let virtual_host = Router.virtual_host
  let make_rate_limit = RateLimiter.make ()
  let log_src = Srv.log_src
  let run = Srv.run
end
