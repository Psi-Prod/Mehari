open Mehari

let file_exists path =
  try
    let _ = Unix.stat path in
    true
  with Unix.Unix_error _ -> false

let read_chunks path =
  let fd =
    Unix.openfile path [ O_RDONLY; O_NONBLOCK ] 0o666 |> Miou_unix.of_file_descr
  in
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  Seq.of_dispenser (fun () ->
      let readed = Miou_unix.read fd buf in
      if readed = 0 then None else Some (Bytes.sub_string buf 0 readed))

let respond_document ?(mime = Mime.app_octet_stream) path =
  if file_exists path then
    let body = read_chunks path |> Body.seq in
    Response.body body mime
  else Response.respond Status.not_found ""

include Private.Static.Make (struct
  module IO = Identity_monad

  type path = string

  let kind path =
    try
      match Unix.stat path with
      | { st_kind = S_REG; _ } -> `Regular_file
      | { st_kind = S_DIR; _ } -> `Directory
      | _ -> `Other
    with
    | Unix.Unix_error _ -> `Other
    | exn -> raise exn

  let exists = file_exists

  let read dirname =
    let dir = Unix.opendir dirname in
    let files =
      Seq.of_dispenser (fun () ->
          try Some (Unix.readdir dir) with End_of_file -> None)
      |> List.of_seq
    in
    Unix.closedir dir;
    files

  let concat = Filename.concat
  let respond_document = respond_document

  let pp_io_err fmt = function
    | Unix.Unix_error (err, fun_name, _) ->
        Format.fprintf fmt "Unix_error %S: %s" fun_name (Unix.error_message err)
    | exn -> raise exn
end)
