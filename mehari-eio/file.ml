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

let default_handler path fname _req =
  let mime =
    Mehari.from_filename fname |> Option.value ~default:Mehari.no_mime
  in
  response_document ~mime path

let default_listing dir files =
  List.map
    (fun fname ->
      "/" ^ Filename.concat dir fname |> Mehari.Gemtext.link ~name:fname)
    files
  |> List.cons (Printf.sprintf "Index: %s" dir |> Mehari.Gemtext.heading `H1)
  |> Mehari.response_gemtext

let not_found = Mehari.(response not_found "")

let static ?(handler = default_handler) ?(listing = default_listing)
    ?(show_hidden = false) base_path req =
  let req_param = Mehari.param req 1 in
  if Filename.is_relative req_param && reference_parent req_param then not_found
  else
    let path = Eio.Path.(base_path / req_param) in
    try
      Eio.Path.with_open_in path (fun flow ->
          match flow#stat.kind with
          | `Regular_file -> handler path req_param req
          | `Directory ->
              Eio.Path.read_dir path
              |> List.filter_map (fun fname ->
                     if
                       (not show_hidden) && String.starts_with ~prefix:"." fname
                     then None
                     else Some fname)
              |> listing req_param
          | _ -> not_found)
    with Eio.Io _ as err ->
      Log.warn (fun log -> log "%a" Eio.Exn.pp err);
      not_found
