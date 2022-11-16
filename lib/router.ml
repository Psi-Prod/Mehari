type t = route list

and route = {
  path : string;
  handler : Request.t -> Response.t Lwt.t;
  rate_limit : Rate_limiter.t option;
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
  | None -> Response.(respond Status.not_found "")
  | Some (handler, None) -> handler req
  | Some (handler, Some limiter) -> (
      match Rate_limiter.check limiter req with
      | None -> handler req
      | Some resp -> resp)

let scope ?rate_limit ?(mw = Fun.id) prefix routes =
  List.concat routes
  |> List.map (fun { path; handler; _ } ->
         { path = prefix ^ path; handler = mw handler; rate_limit })
