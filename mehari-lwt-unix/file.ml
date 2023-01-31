let src = Logs.Src.create "mehari.lwt_unix.static"

module Log = (val Logs.src_log src)
open Lwt.Syntax

let chunk_size = 16384

exception Exited
(* An error occured during CGI script execution. *)

let read_body proc =
  Lwt_seq.unfold_lwt
    (fun finished ->
      if finished then Lwt.return_none
      else
        let* data = Lwt_io.read ~count:chunk_size proc#stdout in
        if String.length data = chunk_size then Lwt.return_some (data, true)
        else
          match proc#state with
          | Lwt_process.Running -> Lwt.return_some (data, true)
          | Exited (WEXITED 0) -> Lwt.return_some (data, false)
          | _ -> raise Exited)
    false

let meta =
  Re.compile Re.(seq [ group (seq [ digit; digit ]); space; group (rep any) ])

let ( let$ ) opt f = match opt with None -> Lwt.return_none | Some x -> f x

let parse_header in_chan =
  match%lwt Lwt_io.read_line_opt in_chan with
  | None -> Lwt.return_none
  | Some header when Bytes.(of_string header |> length) > 1024 ->
      Lwt.return_none
  | Some header ->
      let$ grp = Re.exec_opt meta header in
      let$ code = Re.Group.get grp 1 |> int_of_string_opt in
      Lwt.return_some (code, Re.Group.get grp 2)

module CGI = Mehari.Private.CGI.Make (Ipaddr)

let cgi_err = Mehari_io.respond Mehari.cgi_error ""

let run_cgi ?(timeout = 5.0) ?(nph = false) path req =
  try%lwt
    let* cwd = Lwt_unix.getcwd () in
    let env = CGI.make_env req ~fullpath:(Filename.concat cwd path) ~path in
    let timeout =
      let* () = Lwt_unix.sleep timeout in
      cgi_err
    in
    let cgi_exec =
      let respond =
        Lwt_process.with_process_in ~stderr:`Dev_null ~env (path, [||])
          (fun proc ->
            if nph then
              let* chunks = read_body proc |> Lwt_seq.to_list in
              `Body (String.concat "" chunks) |> Mehari_io.respond_raw
            else
              match%lwt parse_header proc#stdout with
              | None -> Mehari_io.respond Mehari.cgi_error ""
              | Some (code, meta) ->
                  let* chunks = read_body proc |> Lwt_seq.to_list in
                  Mehari_io.respond_raw
                    (`Full (code, meta, String.concat "" chunks)))
      in
      respond
    in
    Lwt.pick [ timeout; cgi_exec ]
  with Exited -> cgi_err

(** TODO: true lazyness (is it even possible?) *)
let rec unfold f u () =
  match%lwt f u with
  | None -> Lwt.return Seq.Nil
  | Some (x, u') ->
      let* xs = unfold f u' () in
      Lwt.return (Seq.Cons (x, fun () -> xs))

let read_chunks ?(chunk_size = 16384) path =
  let+ ic = Lwt_io.open_file path ~mode:Input in
  unfold
    (fun ended ->
      if ended then
        let* () = Lwt_io.close ic in
        Lwt.return_none
      else
        let* chunk = Lwt_io.read ~count:chunk_size ic in
        if String.length chunk = chunk_size - 1 then
          Lwt.return_some (chunk, false)
        else Lwt.return_some (chunk, true))
    false

let not_found = Mehari_io.respond Mehari.not_found ""

let respond_document ?mime path =
  if%lwt Lwt_unix.file_exists path then
    let mime = Option.value mime ~default:Mehari.no_mime in
    let* chunks = read_chunks path in
    let* cs = chunks () in
    Mehari_io.respond_body (Mehari.seq (fun () -> cs)) mime
  else not_found

let from_filename ?(lookup = `Ext) ?charset fname =
  match lookup with
  | `Ext -> Mehari.from_filename ?charset fname |> Lwt.return
  | `Content ->
      let+ content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
      Mehari.from_content ?charset content
  | `Both -> (
      let+ content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
      match Mehari.from_content ?charset content with
      | None -> Mehari.from_filename ?charset fname
      | Some m -> Some m)

include
  Mehari.Private.Static.Make
    (struct
      module IO = Lwt

      type path = string

      let kind path =
        let open Lwt.Infix in
        Lwt_unix.lstat path >|= function
        | { st_kind = S_REG; _ } -> `Regular_file
        | { st_kind = S_DIR; _ } -> `Directory
        | _ -> `Other

      let read path = Lwt_unix.files_of_directory path |> Lwt_stream.to_list
      let concat = Filename.concat
      let response_document = respond_document

      let pp_io_err fmt = function
        | Unix.Unix_error (err, fun_name, _) ->
            Format.fprintf fmt "Unix_error %S: %s" fun_name
              (Unix.error_message err)
        | exn -> raise exn
    end)
    (Ipaddr)
