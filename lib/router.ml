type t = string * (Request.t -> Response.t Lwt.t)

let route r handler = (r, handler)

let router routes req =
  let url = Request.uri req |> Uri.path in
  let handler =
    List.fold_left
      (fun acc (route, handler) ->
        match acc with
        | None -> if String.equal url route then Some handler else None
        | Some _ -> acc)
      None routes
  in
  match handler with
  | None -> Response.(respond Status.not_found "")
  | Some h -> h req
