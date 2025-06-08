open Mehari
open Lwt.Infix
open Lwt.Syntax

let src = Logs.Src.create "mehari.lwt_unix.cgi"

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

let cgi_err = Response.respond Status.cgi_error ""

let make_cgi_env req ~script_path =
  let open Mehari.Private.Cgi in
  make req ~script_path |> to_env
  |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)

let run_cgi ?(timeout = 5.0) ?(non_parsed = false) path req =
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
          if non_parsed then
            Lwt_io.read proc#stdout >|= Response.Private.unsafe_raw
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
      Log.err (fun log ->
          log "Exception occured during CGI script running: %S"
            (Printexc.to_string exn));
      Lwt.reraise exn)
