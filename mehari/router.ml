type domain_handler = {
  domain : [ `host ] Domain_name.t;
  handler : Handler.t;
  all : Handler.t option;
}

type route =
  | Route : {
      path : ('continuation, Handler.t) Path.t;
      handler : 'continuation;
      middlewares : Middleware.t list;
      rate_limiter : Rate_limit.t option;
    }
      -> route

let route ?rate_limit ?(middlewares = []) path handler =
  Route { path; handler; middlewares; rate_limiter = rate_limit }

let not_found = Response.(respond Status.not_found "")

let router routes req =
  let target = Request.target req in
  let rec loop = function
    | Route { path; handler; middlewares; rate_limiter } :: continue ->
        begin match Path.Private.sscanf path target handler with
        | Some handler -> begin
            let handler = Middleware.pipeline middlewares handler in
            match rate_limiter with
            | None -> handler req
            | Some limiter ->
                begin match Rate_limit.check limiter req with
                | None ->
                    Logs.info ~src:Logger.src (fun log ->
                        log "'%a' is rate limited" Ipaddr.pp (Request.ip req));
                    handler req
                | Some resp -> resp
                end
          end
        | None -> loop continue
        end
    | [] -> not_found
  in
  loop routes

let domain ?all domain handler =
  match Result.bind (Domain_name.of_string domain) Domain_name.host with
  | Ok domain -> { domain; handler; all }
  | Error (`Msg msg) ->
      Format.kasprintf invalid_arg "Invalid domain name %S: %s" domain msg

let virtual_host host_handlers req =
  match Request.Private.server_hostname req with
  | Ip_addr _ -> not_found
  | Domain_name requested -> begin
      let matching_domain { domain; handler; all } =
        if Domain_name.equal domain requested then Some handler
        else if Domain_name.is_subdomain ~subdomain:requested ~domain then all
        else None
      in
      match List.find_map matching_domain host_handlers with
      | None -> not_found
      | Some handler -> handler req
    end
