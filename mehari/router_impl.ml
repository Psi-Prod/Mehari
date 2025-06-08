module Make (RateLimiter : Signatures.RATE_LIMITER) (IO : Signatures.IO) :
  Signatures.ROUTER with module IO = IO and type rate_limiter := RateLimiter.t =
struct
  module IO = IO

  type handler = Request.t -> Response.t IO.t
  type middleware = handler -> handler

  type domain_handler = {
    domain : [ `host ] Domain_name.t;
    handler : handler;
    all : handler option;
  }

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

  let not_found = Response.(respond Status.not_found "")

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
                      Logs.info ~src:Logger_impl.src (fun log ->
                          log "'%a' is rate limited" Ipaddr.pp (Request.ip req));
                      handler req
                  | Some resp -> IO.return resp))
          | None -> loop continue)
      | [] -> IO.return not_found
    in
    loop routes

  let domain ?all domain handler =
    match Result.bind (Domain_name.of_string domain) Domain_name.host with
    | Ok domain -> { domain; handler; all }
    | Error (`Msg msg) ->
        Format.kasprintf invalid_arg "Invalid domain name %S: %s" domain msg

  let virtual_host host_handlers req =
    match Request.Private.server_hostname req with
    | `IPAddr _ -> IO.return not_found
    | `DomainName requested -> begin
        let matching_domain { domain; handler; all } =
          if Domain_name.equal domain requested then Some handler
          else if Domain_name.is_subdomain ~subdomain:requested ~domain then all
          else None
        in
        match List.find_map matching_domain host_handlers with
        | None -> IO.return not_found
        | Some handler -> handler req
      end
end
