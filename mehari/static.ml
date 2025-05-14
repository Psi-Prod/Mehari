module Make (Fs : Signatures.FILE_SYSTEM) :
  Signatures.STATIC with module IO := Fs.IO and type dir_path := Fs.path =
struct
  type handler = Request.t -> Response.t Fs.IO.t

  let src = Logs.Src.create "mehari.static"

  module Log = (val Logs.src_log src)

  let ( let* ) = Fs.IO.bind

  let pp_kind fmt = function
    | `Regular_file -> Format.pp_print_string fmt "\u{1F4C4}"
    | `Directory -> Format.pp_print_string fmt "\u{1F4C1}"
    | `Other -> Format.pp_print_string fmt "\u{2753}"

  let default_handler path req =
    let fname = Request.param req 1 in
    let mime =
      match Mime.from_filename fname with
      | None when Filename.check_suffix fname ".gmi" -> Some (Mime.gemini ())
      | (None | Some _) as m -> m
    in
    Fs.respond_document ?mime path

  let parent_path =
    Re.(compile (seq [ Re.group (seq [ rep1 any; char '/' ]); rep1 any ]))

  let default_listing files req =
    let dirs =
      List.map
        (fun (kind, fname) ->
          let name = Format.asprintf "%a %s" pp_kind kind fname in
          Filename.concat (Request.target req) fname |> Gemtext.link ~name)
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
            let name =
              Format.asprintf "%a Parent directory" pp_kind `Directory
            in
            let link = Re.Group.get grp 1 |> Gemtext.link ~name in
            title :: link :: Gemtext.newline :: dirs
    in
    menu |> Response.gemtext |> Fs.IO.return

  let read_dir ~show_hidden ~index path =
    let* files = Fs.read path in
    List.fold_left
      (fun acc fname ->
        let* acc = acc in
        if String.equal fname index then
          `Index (Fs.concat path fname) |> Fs.IO.return
        else
          match acc with
          | `Index _ -> Fs.IO.return acc
          | `Filenames fnames ->
              if (not show_hidden) && String.starts_with ~prefix:"." fname then
                `Filenames fnames |> Fs.IO.return
              else
                let* kind = Fs.concat path fname |> Fs.kind in
                `Filenames ((kind, fname) :: fnames) |> Fs.IO.return)
      (`Filenames [] |> Fs.IO.return)
      files

  let reference_parent path =
    String.fold_left
      (fun (acc, dot) -> function
        | '.' when dot -> (true, dot) | '.' -> (acc, true) | _ -> (acc, dot))
      (false, false) path
    |> fst

  let not_found = Response.(respond Status.not_found "") |> Fs.IO.return

  let static ?(handler = default_handler) ?(dir_listing = default_listing)
      ?(index = "index.gmi") ?(show_hidden = false) base_path req =
    let req_path = Request.param req 1 |> Uri.pct_decode in
    if reference_parent req_path then not_found
    else
      let path = Fs.concat base_path req_path in
      try
        let* is_exists = Fs.exists path in
        if is_exists then
          let* kind = Fs.kind path in
          match kind with
          | `Regular_file -> handler path req
          | `Directory ->
              Fs.IO.bind (read_dir ~show_hidden ~index path) (function
                | `Filenames fnames -> dir_listing fnames req
                | `Index index_path -> handler index_path req)
          | `Other -> not_found
        else not_found
      with io ->
        Log.warn (fun log -> log "%a" Fs.pp_io_err io);
        not_found
end
