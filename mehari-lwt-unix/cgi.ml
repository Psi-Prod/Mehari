open Lwt.Syntax

let chunk_size = 16384

exception Exited

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
          | _ -> Lwt.fail Exited)
    false

let meta = Re.compile Re.(seq [ group (rep1 digit); space; group (rep any) ])
let ( let$ ) opt f = match opt with None -> Lwt.return_none | Some x -> f x

let parse_header in_chan =
  match%lwt Lwt_io.read_line_opt in_chan with
  | None -> Lwt.return_none
  | Some header when Bytes.(of_string header |> length) > 1024 ->
      Lwt.return_none
  | Some header ->
      let$ grp = Re.exec_opt meta header in
      let$ code = int_of_string_opt (Re.Group.get grp 1) in
      Lwt.return_some (code, Re.Group.get grp 2)

let with_proc ?timeout ?env cmd f =
  let proc = Lwt_process.open_process_in ?timeout ?env cmd in
  Lwt.finalize
    (fun () -> f proc)
    (fun () ->
      let* _ = proc#close in
      Lwt.return_unit)

module CGI = Mehari.Private.CGI.Make (Ipaddr)

let run_cgi ?(timeout = 5.0) ?(nph = false) path req =
  try%lwt
    let* cwd = Lwt_unix.getcwd () in
    let env = CGI.make_env req ~fullpath:(Filename.concat cwd path) ~path in
    with_proc ~timeout ~env (path, [||]) (fun proc ->
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
  with Exited -> Mehari_io.respond Mehari.cgi_error ""
