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
    route : bool * string;
    handler : handler;
    rate_limit : RateLimiter.t option;
  }

  let no_route = []

  let route ?rate_limit ?(mw = Fun.id) ?(regex = false) r handler =
    [ { route = (regex, r); handler = mw handler; rate_limit } ]

  let compare_url u u' =
    match (u, u') with
    | "", "/" | "/", "" | "", "" -> true
    | "", _ | _, "" -> false
    | _, _ ->
        if String.equal u u' then true
        else if String.ends_with ~suffix:"/" u then
          String.equal (String.sub u 0 (String.length u - 1)) u'
        else if String.ends_with ~suffix:"/" u' then
          String.equal (String.sub u' 0 (String.length u' - 1)) u
        else false

  let match_ (regex, route) path =
    if regex then `Grp (Re.exec_opt (Re.Perl.re route |> Re.Perl.compile) path)
    else `Bool (compare_url route path)

  let router routes req =
    let routes = List.concat routes in
    let path = Request.target req in
    let route =
      List.fold_left
        (fun acc { route; handler; rate_limit } ->
          match acc with
          | None -> (
              match match_ route path with
              | `Bool true -> Some (handler, rate_limit, None)
              | `Grp (Some _ as g) -> Some (handler, rate_limit, g)
              | `Bool false | `Grp None -> None)
          | Some _ -> acc)
        None routes
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
    |> List.map (fun { route = typ, r; handler; _ } ->
           { route = (typ, prefix ^ r); handler = mw handler; rate_limit })

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
