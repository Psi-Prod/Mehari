module IO = Identity_reader_monad

module Clock = struct
  type t = [ `Clock of float ] Eio.Time.clock

  (* Taken from mirage-clock-unix https://github.com/mirage/mirage-clock/blob/main/unix/pclock.ml#L17 *)
  let ps_count_in_s = 1_000_000_000_000L

  let now_d_ps clock =
    let ns, secs = Eio.Time.now clock |> Float.modf in
    let ns = Int64.of_float (ns *. 1000.) in
    let secs = Int64.of_float secs in
    let days = Int64.div secs 86_400L in
    let rem_s = Int64.rem secs 86_400L in
    let frac_ps = Int64.mul ns 1000L in
    let rem_ps = Int64.mul rem_s ps_count_in_s in
    (Int64.to_int days, Int64.add rem_ps frac_ps)
end

module RateLimiter = Mehari.Private.Rate_limiter_impl.Make (Clock) (IO)

module Logger =
  Mehari.Private.Logger_impl.Make
    (Clock)
    (struct
      include IO

      let finally t f r env = try f (t () env) env with exn -> r exn env
    end)

module Router = Mehari.Private.Router_impl.Make (RateLimiter) (Logger)
module Server = Server_impl.Make (Logger)

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
let respond_document = File.respond_document
let static = File.static
let run = Server.run
