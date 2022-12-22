open Mehari.Private

module type IO_RESPONSE = sig
  val respond : 'a Mehari.status -> 'a -> Mehari.response Lwt.t
  val respond_body : Mehari.body -> Mehari.mime -> Mehari.response Lwt.t
  val respond_text : string -> Mehari.response Lwt.t

  val respond_gemtext :
    ?charset:string ->
    ?lang:string list ->
    Mehari.Gemtext.t ->
    Mehari.response Lwt.t

  val respond_raw :
    [ `Body of string | `Full of int * string * string ] ->
    Mehari.response Lwt.t
end

module type S = sig
  module IO = Lwt
  include Mehari.NET with module IO := IO and type addr = Ipaddr.t
  include IO_RESPONSE
  include Server_impl.S with module IO := IO
end

module Make
    (Clock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S) : S with type stack = Stack.t = struct
  module IO = Lwt
  module Addr = Ipaddr
  module RateLimiter = Rate_limiter_impl.Make (Clock) (IO) (Addr)

  module Logger =
    Logger_impl.Make
      (Clock)
      (struct
        include Lwt

        let finally = try_bind
      end)
      (Addr)

  module Router = Router_impl.Make (RateLimiter) (Logger)
  module Server = Server_impl.Make (Stack) (Time) (Logger)

  type addr = Addr.t
  type handler = Router.handler
  type middleware = handler -> handler
  type route = Router.route
  type rate_limiter = RateLimiter.t
  type stack = Stack.t

  let respond s i = Mehari.response s i |> IO.return
  let respond_body b m = Mehari.response_body b m |> IO.return
  let respond_text t = Mehari.response_text t |> IO.return

  let respond_gemtext ?charset ?lang g =
    Mehari.response_gemtext ?charset ?lang g |> IO.return

  let respond_raw g = Mehari.response_raw g |> IO.return
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
  let run_lwt = Server.run_lwt
  let run = Server.run
end
