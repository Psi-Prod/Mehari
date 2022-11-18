type t = route list

and route = {
  path : string;
  handler : Request.t -> Response.t Lwt.t;
  rate_limit : Rate_limiter.t option;
}

type parse_ctx = {
  offset : int;
  buf : Buffer.t;
  param : bool;
  wildcard : string option;
  tokens : token list;
}

and token = Literal of string | Param of string | Wildcard of string

let match_ tokens route =
  let path = String.split_on_char '/' route |> List.filter (( <> ) "") in
  try
    List.fold_left2
      (fun acc p p' ->
        match (acc, p) with
        | Some acc, Literal l when p' = l -> Some acc
        | Some acc, Param param -> Some ((param, p') :: acc)
        | _, _ -> None)
      (Some []) tokens path
  with Invalid_argument _ -> None

let parse path =
  let rec loop ctx =
    let ctx = { ctx with offset = ctx.offset + 1 } in
    if path = "" || String.length path = ctx.offset then
      let c = Buffer.contents ctx.buf in
      if c = "" then ctx
      else
        {
          ctx with
          tokens =
            (if ctx.param then Param c else Literal c) :: ctx.tokens |> List.rev;
        }
    else
      match String.get path ctx.offset with
      | '*' when ctx.param || Option.is_some ctx.wildcard ->
          "Path wildcard must be just '*'" |> invalid_arg
      | '*' -> loop { ctx with wildcard = Some "" }
      | ':' when ctx.param ->
          Printf.sprintf "Empty path parameter name in '%s'" path |> invalid_arg
      | ':' -> loop { ctx with param = true }
      | '/' when ctx.param ->
          let p = Buffer.contents ctx.buf in
          Buffer.reset ctx.buf;
          loop { ctx with param = false; tokens = Param p :: ctx.tokens }
      | '/' ->
          let p = Buffer.contents ctx.buf in
          Buffer.reset ctx.buf;
          if p = "" then loop ctx
          else loop { ctx with param = false; tokens = Literal p :: ctx.tokens }
      | c when Option.is_some ctx.wildcard ->
          loop
            {
              ctx with
              wildcard = Some (Option.get ctx.wildcard ^ String.make 1 c);
            }
      | c ->
          Buffer.add_char ctx.buf c;
          loop ctx
  in
  let result =
    loop
      {
        offset = -1;
        buf = Buffer.create 101;
        wildcard = None;
        param = false;
        tokens = [];
      }
  in
  match result.wildcard with
  | None -> `Tokens result.tokens
  | Some w -> `Wildcard w

let route ?rate_limit ?(mw = Fun.id) path handler =
  [
    {
      path = Uri.of_string path |> Uri.to_string;
      handler = mw handler;
      rate_limit;
    };
  ]

let interp_params req = function
  | `Assoc a -> Request.attach_params req a
  | `Wildcard w -> Request.set_uri req w

let router routes req =
  let routes = List.concat routes in
  let uri = Request.uri req |> Uri.path in
  let route =
    List.fold_left
      (fun acc { path; handler; rate_limit } ->
        match acc with
        | None -> (
            match parse path with
            | `Wildcard _ as w -> Some (handler, rate_limit, w)
            | `Tokens t -> (
                match match_ t uri with
                | None -> None
                | Some assoc -> Some (handler, rate_limit, `Assoc assoc)))
        | Some _ -> acc)
      None routes
  in
  match route with
  | None -> Response.(respond Status.not_found "")
  | Some (handler, None, p) -> interp_params req p |> handler
  | Some (handler, Some limiter, p) -> (
      match Rate_limiter.check limiter req with
      | None -> interp_params req p |> handler
      | Some resp -> resp)

let scope ?rate_limit ?(mw = Fun.id) prefix routes =
  List.concat routes
  |> List.map (fun { path; handler; _ } ->
         { path = prefix ^ path; handler = mw handler; rate_limit })
