module type S = sig
  type t
  type clock

  val check : t -> Request.t -> Response.t option

  val make :
    clock -> ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
end

module Make (Clock : Signatures.PCLOCK) : S with type clock = Clock.t = struct
  module AddrMap = Stdlib.Map.Make (Ipaddr)

  type clock = Clock.t

  type t = {
    clock : clock;
    requests : int;
    period : int;
    mutable next_timestamp : int;
    mutable history : int AddrMap.t;
  }

  let now clock =
    let _, ps = Clock.now_d_ps clock in
    Int64.div ps (Int64.of_float (10. ** 12.)) |> Int64.to_int

  let reset t =
    t.next_timestamp <- now t.clock + t.period;
    t.history <- AddrMap.empty

  let check t req =
    let time_left = t.next_timestamp - now t.clock in
    if time_left <= 0 then reset t;
    let addr = Request.ip req in
    let n = AddrMap.find_opt addr t.history |> Option.fold ~none:1 ~some:succ in
    t.history <- AddrMap.add addr n t.history;
    if n > t.requests then
      Response.(respond Status.slow_down) "Rate limited" |> Option.some
    else None

  let make clock ?(period = 1) requests duration =
    let period =
      period
      *
      match duration with
      | `Second -> 1
      | `Minute -> 60
      | `Hour -> 3600
      | `Day -> 3600 * 24
    in
    let t =
      { clock; requests; period; next_timestamp = 0; history = AddrMap.empty }
    in
    reset t;
    t
end
