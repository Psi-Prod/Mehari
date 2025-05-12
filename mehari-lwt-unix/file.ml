open Mehari
open Lwt.Infix
open Lwt.Syntax

let src = Logs.Src.create "mehari.lwt_unix.static"

module Log = (val Logs.src_log src)

let meta =
  Re.compile Re.(seq [ group (seq [ digit; digit ]); space; group (rep any) ])

let parse_header in_chan =
  Lwt_io.read_line_opt in_chan >|= function
  | None -> None
  | Some header when Bytes.(of_string header |> length) > 1024 -> None
  | Some header ->
      let ( let* ) = Option.bind in
      let* grp = Re.exec_opt meta header in
      let* code = Re.Group.get grp 1 |> int_of_string_opt in
      Some (code, Re.Group.get grp 2)

let cgi_err = Response.respond Response.Status.cgi_error ""

let make_cgi_env req ~script_path =
  let open Mehari.Private.Cgi in
  make req ~script_path ~server_addr:Ipaddr.(V4 V4.any)
  |> to_env
  |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)

let run_cgi ?(timeout = 5.0) ?(nph = false) path req =
  let run () =
    let* cwd = Lwt_unix.getcwd () in
    let env = make_cgi_env req ~script_path:(Filename.concat cwd path) in
    let timeout =
      let+ () = Lwt_unix.sleep timeout in
      cgi_err
    in
    let cgi_script_exec =
      Lwt_process.with_process_in ~stderr:`Dev_null ~env (path, [||])
        (fun proc ->
          if nph then Lwt_io.read proc#stdout >|= Response.Private.unsafe_raw
          else
            parse_header proc#stdout >>= function
            | None -> Lwt.return cgi_err
            | Some (code, meta) ->
                let+ body = Lwt_io.read proc#stdout in
                Response.Private.raw code meta body)
    in
    Lwt.pick [ timeout; cgi_script_exec ]
  in
  Lwt.catch run (function exn ->
      Logs.err (fun log ->
          log "Exception occured during CGI script running: %S"
            (Printexc.to_string exn));
      Lwt.reraise exn)

(* TODO: true lazyness (is it even possible?) *)
let rec unfold f u () =
  f u >>= function
  | None -> Lwt.return Seq.Nil
  | Some (x, u') ->
      let+ xs = unfold f u' () in
      Seq.Cons (x, fun () -> xs)

let read_chunks path =
  let+ ic = Lwt_io.open_file path ~mode:Input in
  unfold
    (fun ended ->
      if ended then Lwt_io.close ic >|= fun () -> None
      else
        let+ chunk = Lwt_io.read ~count:4096 ic in
        if String.length chunk = 4096 then Some (chunk, false)
        else Some (chunk, true))
    false

let respond_document ?(mime = Mehari.Mime.app_octet_stream) path =
  let* exists = Lwt_unix.file_exists path in
  if exists then
    let* chunks = read_chunks path in
    let+ cs = chunks () in
    Response.body (Response.Body.seq (fun () -> cs)) mime
  else Response.respond Status.not_found "" |> Lwt.return

include Mehari.Private.Static.Make (struct
  module IO = Lwt

  type path = string

  let kind path =
    Lwt.catch
      (fun () ->
        Lwt_unix.lstat path >|= function
        | { st_kind = S_REG; _ } -> `Regular_file
        | { st_kind = S_DIR; _ } -> `Directory
        | _ -> `Other)
      (function Unix.Unix_error _ -> Lwt.return `Other | exn -> raise exn)

  let exists = Lwt_unix.file_exists
  let read path = Lwt_unix.files_of_directory path |> Lwt_stream.to_list
  let concat = Filename.concat
  let respond_document = respond_document

  let pp_io_err fmt = function
    | Unix.Unix_error (err, fun_name, _) ->
        Format.fprintf fmt "Unix_error %S: %s" fun_name (Unix.error_message err)
    | exn -> raise exn
end)
