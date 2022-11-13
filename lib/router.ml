type t = (string * (Request.t -> Response.t Lwt.t)) list

let route r handler = [ (r, handler) ]

let router routes req =
  let routes = List.concat routes in
  let uri = Request.uri req |> Uri.path in
  let handler =
    List.fold_left
      (fun acc (route, handler) ->
        match acc with
        | None -> if String.equal uri route then Some handler else None
        | Some _ -> acc)
      None routes
  in
  match handler with
  | None -> Response.(respond Status.not_found "")
  | Some h -> h req

let scope prefix routes =
  List.concat routes |> List.map (fun (r, h) -> (prefix ^ r, h))
