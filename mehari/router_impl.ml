module type S = sig
  module IO : Types.IO

  type route
  type rate_limiter
  type addr
  type handler = addr Handler.Make(IO).t
  type middleware = handler -> handler

  val router : route list -> handler

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?typ:[ `Raw | `Regex ] ->
    string ->
    handler ->
    route

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route
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
    route : [ `Raw | `Regex ] * string;
    handler : handler;
    rate_limit : RateLimiter.t option;
  }

  let route ?rate_limit ?(mw = Fun.id) ?(typ = `Raw) r handler =
    [ { route = (typ, r); handler = mw handler; rate_limit } ]

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

  let match_ (typ, route) path =
    match typ with
    | `Raw -> `Bool (compare_url route path)
    | `Regex -> `Grp (Re.exec_opt (Re.Perl.re route |> Re.Perl.compile) path)

  let router routes req =
    let routes = List.concat routes in
    let path = Request.uri req |> Uri.path in
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
    | None ->
        Logger.info (fun log ->
            log "respond not found for path '%a' to '%a'." Uri.pp
              (Request.uri req) Addr.pp (Request.ip req));
        Response.(response Status.not_found "") |> IO.return
    | Some (handler, limit_opt, params) -> (
        let req = Request.attach_params req params in
        Logger.info (fun log ->
            log "serve '%a' for '%a'" Uri.pp (Request.uri req) Addr.pp
              (Request.ip req));
        match limit_opt with
        | None -> handler req
        | Some limiter -> (
            match RateLimiter.check limiter req with
            | None ->
                Logger.info (fun log ->
                    log "'%a' is rate limited." Addr.pp (Request.ip req));
                handler req
            | Some resp -> resp))

  let scope ?rate_limit ?(mw = Fun.id) prefix routes =
    List.concat routes
    |> List.map (fun { route = typ, r; handler; _ } ->
           { route = (typ, prefix ^ r); handler = mw handler; rate_limit })
end
