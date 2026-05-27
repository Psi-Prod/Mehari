module Addr_map = Map.Make (Ipaddr)

type t = {
  requests : int;
  period : Ptime.span;
  mutable next_timestamp : Ptime.t;
  mutable history : int Addr_map.t;
}

let reset t =
  t.next_timestamp <- Option.get (Ptime.add_span (Mirage_ptime.now ()) t.period);
  t.history <- Addr_map.empty

let check t req =
  if Ptime.is_later (Mirage_ptime.now ()) ~than:t.next_timestamp then reset t;
  let addr = Request.ip req in
  let n = Addr_map.find_opt addr t.history |> Option.fold ~none:1 ~some:succ in
  t.history <- Addr_map.add addr n t.history;
  if n > t.requests then
    Response.(respond Status.slow_down) "Rate limited" |> Option.some
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
  let t =
    {
      requests;
      period = Ptime.Span.of_int_s period;
      next_timestamp = Ptime.epoch;
      history = Addr_map.empty;
    }
  in
  reset t;
  t
