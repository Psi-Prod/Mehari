module type S = sig
  type t
  type route
  type rate_limiter

  val route :
    ?rate_limiter:rate_limiter ->
    ?mw:(Handler.t -> Handler.t) ->
    string ->
    Handler.t ->
    route

  val router : route list -> Request.t -> t

  val scope :
    ?rate_limit:rate_limiter ->
    ?mw:(Handler.t -> Handler.t) ->
    string ->
    route list
end

module Make (RateLimiter : Rate_limiter_impl.S) (Clock : Mirage_clock.PCLOCK) =
struct
  module Logger = Logger_impl.Make (Clock)

  type t = route list

  and route = {
    path : string;
    handler : Request.t -> Response.t Lwt.t;
    rate_limit : RateLimiter.t option;
  }

  let route ?rate_limit ?(mw = Fun.id) path handler =
    [
      {
        path = Uri.of_string path |> Uri.to_string;
        handler = mw handler;
        rate_limit;
      };
    ]

  let router routes req =
    let routes = List.concat routes in
    let uri = Request.uri req |> Uri.path in
    let route =
      List.fold_left
        (fun acc { path; handler; rate_limit } ->
          match acc with
          | None ->
              if String.equal uri path then Some (handler, rate_limit) else None
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
    |> List.map (fun { path; handler; _ } ->
           { path = prefix ^ path; handler = mw handler; rate_limit })
end
