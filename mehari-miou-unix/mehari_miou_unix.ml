open Mehari

module Clock = struct
  type t = unit

  (* Taken from mirage-clock-unix https://github.com/mirage/mirage-clock/blob/main/unix/pclock.ml#L17 *)
  let ps_count_in_s = 1_000_000_000_000L

  let now_d_ps clock =
    let ns, secs = Unix.gettimeofday clock |> Float.modf in
    let ns = Int64.of_float (ns *. 1000.) in
    let secs = Int64.of_float secs in
    let days = Int64.div secs 86_400L in
    let rem_s = Int64.rem secs 86_400L in
    let frac_ps = Int64.mul ns 1000L in
    let rem_ps = Int64.mul rem_s ps_count_in_s in
    (Int64.to_int days, Int64.add rem_ps frac_ps)
end

module RateLimiter = Private.Rate_limiter_impl.Make (Clock) (Identity_monad)

module Logger =
  Private.Logger_impl.Make
    (Clock)
    (struct
      include Identity_monad

      let finally t f r = try f (t ()) with exn -> r exn
    end)

module Router = Private.Router_impl.Make (RateLimiter) (Logger)

type handler = Router.handler
type middleware = handler -> handler
type route = Router.route
type rate_limiter = RateLimiter.t

let set_log_lvl = Logger.set_level
let logger = Logger.logger ()
let debug = Logger.debug
let info = Logger.info
let warning = Logger.warning
let error = Logger.error
let pipeline = Router.pipeline
let router = Router.router
let route = Router.route
let virtual_hosts = Router.virtual_hosts
let make_rate_limit = RateLimiter.make ()
let respond_document = File.respond_document
let static = File.static
let run_cgi = Cgi.run_cgi

module Config = Config

let run = Server.run
