open Mehari

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

module RateLimiter =
  Private.Rate_limiter_impl.Make (Clock) (Identity_reader_monad)

module Logger =
  Private.Logger_impl.Make
    (Clock)
    (struct
      include Identity_reader_monad

      let finally t f r env = try f (t () env) env with exn -> r exn env
    end)

module Router = Private.Router_impl.Make (RateLimiter) (Logger)
module Identity_reader_monad = Identity_reader_monad

type handler = Router.handler
type middleware = handler -> handler
type domain_handler = Router.domain_handler
type route = Router.route
type rate_limiter = RateLimiter.t

let logger = Logger.logger
let pipeline = Router.pipeline
let router = Router.router
let route = Router.route
let domain = Router.domain
let virtual_host = Router.virtual_host
let make_rate_limit = RateLimiter.make
let respond_document = File.respond_document
let static = File.static
let run_cgi = Cgi.run_cgi

module Config = Config

let log_src = Server.log_src
let run = Server.run
