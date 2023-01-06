module type DIR = sig
  module IO : Types.IO

  type path

  val kind : path -> [ `Regular_file | `Directory | `Other ] IO.t
  val read : path -> string list IO.t
  val concat : path -> string -> path
  val response_document : ?mime:Mime.t -> path -> Response.t IO.t
  val pp_io_err : Format.formatter -> exn -> unit
end

module type S = sig
  module IO : Types.IO

  type addr
  type handler = addr Handler.Make(IO).t
  type dir_path

  val static :
    ?handler:(dir_path -> handler) ->
    ?dir_listing:(string list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    dir_path ->
    handler
end

module Make (Dir : DIR) (Addr : Types.T) :
  S
    with module IO := Dir.IO
     and type addr := Addr.t
     and type dir_path := Dir.path = struct
  type handler = Addr.t Handler.Make(Dir.IO).t

  let src = Logs.Src.create "mehari.static"

  module Log = (val Logs.src_log src)

  let default_handler path req =
    let fname = Request.param req 1 in
    let mime =
      match Mime.from_filename fname with
      | None when Filename.check_suffix fname ".gmi" -> Mime.gemini ()
      | None -> Mime.no_mime
      | Some m -> m
    in
    Dir.response_document ~mime path

  let parent_path =
    Re.(compile (seq [ Re.group (seq [ rep1 any; char '/' ]); rep1 any ]))

  let default_listing files req =
    let dirs =
      List.map
        (fun fname ->
          Filename.concat (Request.target req) fname |> Gemtext.link ~name:fname)
        files
    in
    let title =
      Request.param req 1 |> Printf.sprintf "Index: %s" |> Gemtext.heading `H1
    in
    let menu =
      if Request.target req = "" then title :: dirs
      else
        match Request.uri req |> Uri.to_string |> Re.exec_opt parent_path with
        | None -> title :: dirs
        | Some grp ->
            let link =
              Re.Group.get grp 1 |> Gemtext.link ~name:"Parent directory"
            in
            title :: link :: Gemtext.newline :: dirs
    in
    menu |> Response.response_gemtext |> Dir.IO.return

  let read_dir ~show_hidden ~index path =
    Dir.IO.bind (Dir.read path) (fun files ->
        List.fold_left
          (fun acc fname ->
            if String.equal fname index then `Index (Dir.concat path fname)
            else
              match acc with
              | `Index _ -> acc
              | `Filenames fnames ->
                  if (not show_hidden) && String.starts_with ~prefix:"." fname
                  then `Filenames fnames
                  else `Filenames (fname :: fnames))
          (`Filenames []) files
        |> Dir.IO.return)

  let reference_parent path =
    String.fold_left
      (fun (acc, dot) -> function
        | '.' when dot -> (true, dot)
        | '.' -> (acc, true)
        | _ -> (acc, dot))
      (false, false) path
    |> fst

  let not_found = Response.(response Status.not_found "") |> Dir.IO.return

  let static ?(handler = default_handler) ?(dir_listing = default_listing)
      ?(index = "index.gmi") ?(show_hidden = false) base_path req =
    let req_path = Request.param req 1 in
    if reference_parent req_path then not_found
    else
      let path = Dir.concat base_path req_path in
      try
        Dir.IO.bind (Dir.kind path) (function
          | `Regular_file -> handler path req
          | `Directory ->
              Dir.IO.bind (read_dir ~show_hidden ~index path) (function
                | `Filenames fnames -> dir_listing fnames req
                | `Index index_path -> handler index_path req)
          | `Other -> not_found)
      with io ->
        Log.warn (fun log -> log "%a" Dir.pp_io_err io);
        not_found
end
