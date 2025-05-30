module Make
    (RateLimiter : Signatures.RATE_LIMITER)
    (Logger : Signatures.LOGGER) :
  Signatures.ROUTER
    with module IO = RateLimiter.IO
     and type rate_limiter := RateLimiter.t = struct
  module IO = RateLimiter.IO

  type handler = Request.t -> Response.t IO.t
  type middleware = handler -> handler

  type route =
    | Route : {
        path : ('continuation, handler) Path.t;
        handler : 'continuation;
        middlewares : middleware list;
        rate_limiter : RateLimiter.t option;
      }
        -> route

  let route ?rate_limit ?(middlewares = []) path handler =
    Route { path; handler; middlewares; rate_limiter = rate_limit }

  let rec pipeline mws handler =
    match mws with [] -> handler | m :: ms -> m (pipeline ms handler)

  let router routes req =
    let target = Request.target req in
    let rec loop = function
      | Route { path; handler; middlewares; rate_limiter } :: continue -> (
          match Path.Private.sscanf path target handler with
          | Some handler -> (
              let handler = pipeline middlewares handler in
              match rate_limiter with
              | None -> handler req
              | Some limiter -> (
                  match RateLimiter.check limiter req with
                  | None ->
                      Logger.info (fun log ->
                          log "'%a' is rate limited" Ipaddr.pp (Request.ip req));
                      handler req
                  | Some resp -> resp))
          | None -> loop continue)
      | [] -> Response.(respond Status.not_found "") |> IO.return
    in
    loop routes

  let virtual_hosts host_handlers req =
    let hostname = Request.Private.server_hostname req in
    let _, handler =
      List.find (fun (host, _) -> String.equal hostname host) host_handlers
    in
    handler req
end
