module type S = sig
  module IO : Types.IO

  type t

  module Addr : Types.ADDR

  val check : t -> Addr.t Request.t -> Response.t IO.t option
  val make : ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
end

module Make (Clock : Mirage_clock.PCLOCK) (IO : Types.IO) (Addr : Types.ADDR) :
  S with module IO = IO and module Addr = Addr = struct
  module IO = IO
  module Addr = Addr
  module AddrMap = Stdlib.Map.Make (Addr)

  type t = {
    mutable requests : int;
    period : int;
    mutable next_timestamp : int;
    mutable history : int AddrMap.t;
  }

  let now () =
    let _, ps = Clock.now_d_ps () in
    Int64.div ps (Int64.of_float (10. ** 12.)) |> Int64.to_int

  let reset t =
    t.next_timestamp <- now () + t.period;
    t.history <- AddrMap.empty

  let check t req =
    let time_left = t.next_timestamp - now () in
    if time_left <= 0 then reset t;
    let addr = Request.ip req in
    let n = AddrMap.find_opt addr t.history |> Option.fold ~none:1 ~some:succ in
    t.history <- AddrMap.add addr n t.history;
    if n > t.requests then
      Response.(response Status.slow_down time_left) |> IO.return |> Option.some
    else None

  let make ?(period = 1) requests duration =
    let period =
      period
      *
      match duration with
      | `Second -> 1
      | `Minute -> 60
      | `Hour -> 3600
      | `Day -> 3600 * 24
    in
    let t = { requests; period; next_timestamp = 0; history = AddrMap.empty } in
    reset t;
    t
end
