module type S = sig
  type t
  type rate_limiter

  val router : t list -> Handler.t

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:(Handler.t -> Handler.t) ->
    ?typ:[ `Raw | `Regex ] ->
    string ->
    Handler.t ->
    t

  val scope :
    ?rate_limit:rate_limiter ->
    ?mw:(Handler.t -> Handler.t) ->
    string ->
    t list ->
    t
end

module Make (RateLimiter : Rate_limiter_impl.S) (Clock : Mirage_clock.PCLOCK) 
  S with type rate_limiter := RateLimiter.t = struct
  module Logger = Logger_impl.Make (Clock)
  
  type t = route list

  and route = {
    route : [ `Raw | `Regex ] * string;
    handler : Handler.t;
    rate_limit : RateLimiter.t option;
  }

  let route ?rate_limit ?(mw = Fun.id) ?(typ = `Raw) r handler =
    [ { route = (typ, r); handler = mw handler; rate_limit } ]

  let match_ (typ, route) path =
    match typ with
    | `Raw -> `Bool (String.equal (Uri.of_string route |> Uri.to_string) path)
    | `Regex -> `Grp (Re.exec_opt (Re.Perl.re route |> Re.Perl.compile) path)

  let router routes req =
    let routes = List.concat routes in
    let uri = Request.uri req |> Uri.path in
    let route =
      List.fold_left
        (fun acc { route; handler; rate_limit } ->
          match acc with
          | None -> (
              match match_ route uri with
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
              (Request.uri req) Ipaddr.pp (Request.ip req));
        Response.(respond Status.not_found "")
    | Some (handler, limit_opt) -> (
        Logger.info (fun log ->
            log "serve '%a' for '%a'" Uri.pp (Request.uri req) Ipaddr.pp
              (Request.ip req));
        match limit_opt with
        | None -> handler req
        | Some limiter -> (
            match RateLimiter.check limiter req with
            | None ->
                Logger.info (fun log ->
                    log "'%a' is rate limited." Ipaddr.pp (Request.ip req));
                handler req
            | Some resp -> resp))

  let scope ?rate_limit ?(mw = Fun.id) prefix routes =
    List.concat routes
    |> List.map (fun { route = typ, r; handler; _ } ->
           { route = (typ, prefix ^ r); handler = mw handler; rate_limit })
end
