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

let not_found = Mehari.(response not_found "")

let safe_resp ?(mime_inference = true) path top_path =
  let body = Eio.Path.load path |> Mehari.string in
  let mime =
    if mime_inference then
      Mehari.from_filename top_path |> Option.value ~default:Mehari.no_mime
    else Mehari.no_mime
  in
  Mehari.response_body body mime

let dir_listing ~show_hidden dir fullpath =
  Eio.Path.read_dir dir
  |> List.filter_map (fun fname ->
         if (not show_hidden) && String.starts_with ~prefix:"." fname then None
         else
           Filename.concat fullpath fname
           |> Mehari.Gemtext.link ~name:fname
           |> Option.some)
  |> List.cons (Mehari.Gemtext.heading `H1 "Index")
  |> Mehari.response_gemtext

let reference_parent path =
  String.fold_left
    (fun (acc, dot) -> function
      | '.' when dot -> (true, dot)
      | '.' -> (acc, true)
      | _ -> (acc, dot))
    (false, false) path
  |> fst

let static ?(show_hidden = false) base_path req =
  let req_path = Mehari.param req 1 in
  if Filename.is_relative req_path && reference_parent req_path then not_found
  else
    let path = Eio.Path.(base_path / req_path) in
    try
      Eio.Path.with_open_in path (fun flow ->
          match flow#stat.kind with
          | `Regular_file | `Symbolic_link -> safe_resp path req_path
          | `Directory -> dir_listing ~show_hidden path req_path
          | _ -> not_found)
    with Eio.Io _ as err ->
      Log.warn (fun log -> log "%a" Eio.Exn.pp err);
      not_found
