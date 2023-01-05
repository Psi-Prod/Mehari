let src = Logs.Src.create "mehari.eio.static"

module Log = (val Logs.src_log src)

let response_document ?mime path =
  let chunk_size = 16384 in
  let body =
    Mehari.stream (fun consume ->
        Eio.Path.with_open_in path (fun flow ->
            let buf = Eio.Buf_read.of_flow flow ~max_size:max_int in
            let n = ref 0 in
            let rec loop () =
              let chunk =
                Eio.Buf_read.take_while
                  (fun _ ->
                    incr n;
                    !n <> chunk_size)
                  buf
              in
              if String.length chunk = chunk_size then (
                consume chunk;
                n := 0;
                loop ())
              else consume chunk
            in
            loop ()))
  in
  Option.value mime ~default:Mehari.no_mime |> Mehari.response_body body

let reference_parent path =
  String.fold_left
    (fun (acc, dot) -> function
      | '.' when dot -> (true, dot)
      | '.' -> (acc, true)
      | _ -> (acc, dot))
    (false, false) path
  |> fst

let default_handler path req =
  let fname = Mehari.param req 1 in
  let mime =
    match Mehari.from_filename fname with
    | None when Filename.check_suffix fname ".gmi" -> Mehari.gemini ()
    | None -> Mehari.no_mime
    | Some m -> m
  in
  response_document ~mime path

let default_listing files req =
  let dirs =
    List.map
      (fun fname ->
        Filename.concat (Mehari.target req) fname
        |> Mehari.Gemtext.link ~name:fname)
      files
  in
  let title =
    Mehari.param req 1 |> Printf.sprintf "Index: %s"
    |> Mehari.Gemtext.heading `H1
  in
  title :: dirs |> Mehari.response_gemtext

let not_found = Mehari.(response not_found "")

let read_dir ~show_hidden path index =
  let open Either in
  Eio.Path.read_dir path
  |> List.fold_left
       (fun acc fname ->
         if String.equal fname index then Right Eio.Path.(path / fname)
         else
           Either.map_left
             (fun l ->
               if (not show_hidden) && String.starts_with ~prefix:"." fname then
                 l
               else fname :: l)
             acc)
       (Left [])

let static ?(handler = default_handler) ?(dir_listing = default_listing)
    ?(index = "index.gmi") ?(show_hidden = false) base_path req =
  let req_path = Mehari.param req 1 in
  if reference_parent req_path then not_found
  else
    let path = Eio.Path.(base_path / req_path) in
    try
      Eio.Path.with_open_in path (fun flow ->
          match flow#stat.kind with
          | `Regular_file -> handler path req
          | `Directory -> (
              match read_dir ~show_hidden path index with
              | Either.Left files -> dir_listing files req
              | Right index_path -> handler index_path req)
          | _ -> not_found)
    with Eio.Io _ as err ->
      Log.warn (fun log -> log "%a" Eio.Exn.pp err);
      not_found
