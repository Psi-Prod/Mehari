module AddrMap = Map.Make (struct
  type t = Unix.inet_addr

  let compare = Stdlib.compare
end)

type t = {
  mutable requests : int;
  period : int;
  mutable next_timestamp : float;
  mutable history : int AddrMap.t;
}

let reset t = t.next_timestamp <- Unix.time () +. Int.to_float t.period

let check t req =
  let time_left = t.next_timestamp -. Unix.time () in
  if time_left < 0. then reset t;
  let addr = Request.addr req in
  let n =
    match AddrMap.find_opt addr t.history with None -> 1 | Some n -> n + 1
  in
  t.history <- AddrMap.add addr n t.history;
  if n > t.requests then
    let seconds = Float.to_int time_left in
    let msg = Printf.sprintf "Wait %i seconds" seconds in
    Response.(respond (Status.slow_down seconds) msg) |> Option.some
  else None

let to_middleware rl handler req =
  match check rl req with None -> handler req | Some resp -> resp

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
  let t = { requests; period; next_timestamp = 0.; history = AddrMap.empty } in
  reset t;
  to_middleware t
