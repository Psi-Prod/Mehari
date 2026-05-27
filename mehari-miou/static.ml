open Mehari

let log_src = Logs.Src.create "mehari-miou.static"

module Log = (val Logs.src_log log_src)

type path = string

let file_exists path =
  try
    let _ = Unix.stat path in
    true
  with Unix.Unix_error _ -> false

let respond_document ?(mime = Mime.app_octet_stream) path =
  let not_found = Response.respond Status.not_found "" in
  if file_exists path then
    begin try
      let file_chunks = Flux.(Stream.from (Source.file ~filename:path 4096)) in
      Response.body (Body.stream file_chunks) mime
    with Sys_error msg ->
      Log.warn (fun log ->
          log {|A sys error occured during file serving "%s": %s|} path msg);
      not_found
    end
  else not_found

let default_handler fname path _ =
  let mime =
    match Mime.from_filename fname with
    | None when Filename.check_suffix fname ".gmi" -> Some (Mime.gemini ())
    | (None | Some _) as m -> m
  in
  respond_document ?mime path

let file_kind path =
  try
    match Unix.stat path with
    | { st_kind = S_REG; _ } -> `Regular_file
    | { st_kind = S_DIR; _ } -> `Directory
    | _ -> `Other
  with Unix.Unix_error _ -> `Other

let pp_kind fmt = function
  | `Regular_file -> Format.pp_print_string fmt "\u{1F4C4}"
  | `Directory -> Format.pp_print_string fmt "\u{1F4C1}"
  | `Other -> Format.pp_print_string fmt "\u{2753}"

let default_listing fname files req =
  let dirs =
    List.map
      (fun (kind, fname) ->
        let name = Format.asprintf "%a %s" pp_kind kind fname in
        let url = Filename.concat (Request.target req) fname in
        Gemtext.link ~name url)
      files
  in
  let title = Printf.sprintf "Index: %s" fname |> Gemtext.heading `H1 in
  let menu =
    if Request.target req = "" then dirs
    else
      let parent_url = Request.uri req |> Uri.to_string |> Filename.dirname in
      let name = Format.asprintf "%a Parent directory" pp_kind `Directory in
      Gemtext.link ~name parent_url :: Gemtext.newline :: dirs
  in
  title :: menu |> Response.gemtext

let read dirname =
  let dir = Unix.opendir dirname in
  let files =
    Seq.of_dispenser (fun () ->
        try Some (Unix.readdir dir) with End_of_file -> None)
    |> List.of_seq
  in
  Unix.closedir dir;
  files

let read_dir ~show_hidden ~index path =
  let files = read path in
  List.fold_left
    (fun acc fname ->
      if String.equal fname index then `Index (Filename.concat path fname)
      else
        match acc with
        | `Index _ -> acc
        | `Filenames fnames ->
            if (not show_hidden) && String.starts_with ~prefix:"." fname then
              `Filenames fnames
            else
              let kind = Filename.concat path fname |> file_kind in
              `Filenames ((kind, fname) :: fnames))
    (`Filenames []) files

let reference_parent path =
  String.fold_left
    (fun (acc, dot) -> function
      | '.' when dot -> (true, dot) | '.' -> (acc, true) | _ -> (acc, dot))
    (false, false) path
  |> fst

let not_found = Response.(respond Status.not_found "")

let file_type path =
  try
    match Unix.stat path with
    | { st_kind = S_REG; _ } -> `Regular_file
    | { st_kind = S_DIR; _ } -> `Directory
    | _ -> `Other
  with Unix.Unix_error _ -> `Other

let static ?handler ?dir_listing ?(index = "index.gmi") ?(show_hidden = false)
    base_path target req =
  let target = Uri.pct_decode target in
  if reference_parent target then not_found
  else
    let path = Filename.concat base_path target in
    try
      if file_exists path then
        let handler =
          match handler with
          | None -> default_handler target
          | Some handler -> handler
        in
        let dir_listing =
          match dir_listing with
          | None -> default_listing target
          | Some handler -> handler
        in
        match file_type path with
        | `Regular_file -> handler path req
        | `Directory ->
            begin match read_dir ~show_hidden ~index path with
            | `Filenames fnames -> dir_listing fnames req
            | `Index index_path -> handler index_path req
            end
        | `Other -> not_found
      else not_found
    with Unix.Unix_error (err, fun_name, _) ->
      Log.warn (fun log ->
          log "Unix_error %S: %s" fun_name (Unix.error_message err));
      not_found
