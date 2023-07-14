let src = Logs.Src.create "mehari.lwt_unix.static"

module Log = (val Logs.src_log src)
open Lwt.Infix
open Lwt.Syntax

exception Exited
(* An error occured during CGI script execution. *)

let read_body proc =
  Lwt_seq.unfold_lwt
    (fun finished ->
      if finished then Lwt.return_none
      else
        let+ data = Lwt_io.read ~count:4096 proc#stdout in
        if String.length data = 4096 then Some (data, true)
        else
          match proc#state with
          | Lwt_process.Running -> Some (data, true)
          | Exited (WEXITED 0) -> Some (data, false)
          | _ -> raise Exited)
    false

let meta =
  Re.compile Re.(seq [ group (seq [ digit; digit ]); space; group (rep any) ])

let ( let$ ) = Option.bind

let parse_header in_chan =
  Lwt_io.read_line_opt in_chan >|= function
  | None -> None
  | Some header when Bytes.(of_string header |> length) > 1024 -> None
  | Some header ->
      let$ grp = Re.exec_opt meta header in
      let$ code = Re.Group.get grp 1 |> int_of_string_opt in
      Some (code, Re.Group.get grp 2)

module CGI = Mehari.Private.CGI.Make (Ipaddr)

let cgi_err = Mehari_io.respond Mehari.cgi_error ""

let run_cgi ?(timeout = 5.0) ?(nph = false) path req =
  Lwt.catch
    (fun () ->
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
                parse_header proc#stdout >>= function
                | None -> Mehari_io.respond Mehari.cgi_error ""
                | Some (code, meta) ->
                    let* chunks = read_body proc |> Lwt_seq.to_list in
                    Mehari_io.respond_raw
                      (`Full (code, meta, String.concat "" chunks)))
        in
        respond
      in
      Lwt.pick [ timeout; cgi_exec ])
    (function Exited -> cgi_err | exn -> raise exn)

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

let not_found = Mehari_io.respond Mehari.not_found ""

let respond_document ?mime path =
  let* exists = Lwt_unix.file_exists path in
  if exists then
    let mime = Option.value mime ~default:Mehari.no_mime in
    let* chunks = read_chunks path in
    let* cs = chunks () in
    Mehari_io.respond_body (Mehari.seq (fun () -> cs)) mime
  else not_found

include
  Mehari.Private.Static.Make
    (struct
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
      let response_document = respond_document

      let pp_io_err fmt = function
        | Unix.Unix_error (err, fun_name, _) ->
            Format.fprintf fmt "Unix_error %S: %s" fun_name
              (Unix.error_message err)
        | exn -> raise exn
    end)
    (Ipaddr)
