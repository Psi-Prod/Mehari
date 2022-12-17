module type S = sig
  module IO : Io.S

  type t

  module Addr : Types.ADDR

  val check : t -> Addr.t Request.t -> Response.t IO.t option
  val make : ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
end

module Make (Clock : Mirage_clock.PCLOCK) (IO : Io.S) (Addr : Types.ADDR) :
  S with module IO = IO and module Addr = Addr = struct
  module IO = IO
  module Addr = Addr
  module AddrMap = Map.Make (Addr)

  type t = {
    mutable requests : int;
    period : int;
    mutable next_timestamp : int;
    mutable history : int AddrMap.t;
  }

  let reset t =
    let now, _ = Clock.now_d_ps () in
    t.next_timestamp <- now + t.period;
    t.history <- AddrMap.empty

  let check t req =
    let now, _ = Clock.now_d_ps () in
    let time_left = t.next_timestamp - now in
    if time_left < 0 then reset t;
    let addr = Request.ip req in
    let n =
      match AddrMap.find_opt addr t.history with None -> 1 | Some n -> n + 1
    in
    t.history <- AddrMap.add addr n t.history;
    if n > t.requests then
      let msg = Printf.sprintf "Wait %i seconds" time_left in
      Response.(response (Status.slow_down time_left) msg)
      |> IO.return |> Option.some
    else None

  let make ?(period = 1) requests duration =
    let period =
      period
      *
      match duration with
      | `Second -> 1
      | `Minute -> 60
      | `Hour -> 60 * 60
      | `Day -> 60 * 60 * 24
    in
    let t = { requests; period; next_timestamp = 0; history = AddrMap.empty } in
    reset t;
    t
end
