module type S = sig
  module IO : Types.IO

  type route
  type rate_limiter
  type addr
  type handler = addr Handler.Make(IO).t
  type middleware = handler -> handler

  val no_middleware : middleware
  val pipeline : middleware list -> middleware
  val router : route list -> handler

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?regex:bool ->
    string ->
    handler ->
    route

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route

  val no_route : route

  val virtual_hosts :
    ?meth:[ `ByURL | `SNI ] -> (string * handler) list -> handler
end

module Make (RateLimiter : Rate_limiter_impl.S) (Logger : Logger_impl.S) :
  S
    with module IO = RateLimiter.IO
     and type rate_limiter := RateLimiter.t
     and type addr := RateLimiter.Addr.t = struct
  module IO = RateLimiter.IO
  module Addr = RateLimiter.Addr

  type handler = Addr.t Handler.Make(IO).t
  type middleware = handler -> handler

  type route = route' list

  and route' = {
    route : [ `Regex of Re.re | `Literal ] * string;
    handler : handler;
    rate_limit : RateLimiter.t option;
  }

  let no_route = []

  let route ?rate_limit ?(mw = Fun.id) ?(regex = false) r handler =
    let kind =
      if regex then `Regex (Re.Perl.re r |> Re.Perl.compile) else `Literal
    in
    [ { route = (kind, r); handler = mw handler; rate_limit } ]

  let compare_url u u' =
    match (u, u') with
    | "", "/" | "/", "" | "", "" -> true
    | "", _ | _, "" -> false
    | _, _ when String.equal u u' -> true
    | _, _ when String.ends_with ~suffix:"/" u ->
        String.equal (String.sub u 0 (String.length u - 1)) u'
    | _, _ when String.ends_with ~suffix:"/" u' ->
        String.equal (String.sub u' 0 (String.length u' - 1)) u
    | _, _ -> false

  let router routes req =
    let routes = List.concat routes in
    let path = Request.target req in
    let route =
      let rec loop = function
        | [] -> None
        | { route = `Regex re, _; handler; rate_limit } :: rs -> (
            match Re.exec_opt re path with
            | None -> loop rs
            | Some _ as grp -> Some (handler, rate_limit, grp))
        | { route = `Literal, r; handler; rate_limit } :: _
          when compare_url r path ->
            Some (handler, rate_limit, None)
        | { route = `Literal, _; _ } :: rs -> loop rs
      in
      loop routes
    in
    match route with
    | None -> Response.(response Status.not_found "") |> IO.return
    | Some (handler, limit_opt, params) -> (
        let req = Request.attach_params req params in
        match limit_opt with
        | None -> handler req
        | Some limiter -> (
            match RateLimiter.check limiter req with
            | None ->
                Logger.info (fun log ->
                    log "'%a' is rate limited" Addr.pp (Request.ip req));
                handler req
            | Some resp -> resp))

  let scope ?rate_limit ?(mw = Fun.id) prefix routes =
    List.concat routes
    |> List.map (fun { route = kind, r; handler; _ } ->
           let r = prefix ^ r in
           let kind =
             match kind with
             | `Regex _ ->
                 `Regex (Re.Perl.re r |> Re.Perl.compile)
                 (* Recompile route to add given prefix. *)
             | `Literal as l -> l
           in
           { route = (kind, r); handler = mw handler; rate_limit })

  let virtual_hosts ?(meth = `SNI) domains_handler req =
    let req_host =
      match meth with
      | `SNI -> Request.sni req
      | `ByURL ->
          Request.uri req |> Uri.host
          |> Option.get (* Guaranteed by [Protocol.make_request]. *)
    in
    match List.find_opt (fun (d, _) -> d = req_host) domains_handler with
    | None -> assert false (* Guaranteed by [Protocol.make_request]. *)
    | Some (_, handler) -> handler req

  let no_middleware h req = h req

  let rec pipeline mws handler =
    match mws with [] -> handler | m :: ms -> m (pipeline ms handler)
end
