open Lwt.Syntax

(* TODO env *)
(* TODO chunked *)

let make_env _req = Array.append [||] [| ("GATEWAY_INTERFACE", "CGI/1.1") |]
let meta = Re.compile Re.(seq [ group (rep1 digit); space; group (rep any) ])
let ( let$ ) opt f = match opt with None -> Lwt.return_none | Some x -> f x

let cgi_output path =
  let temp_path = Filename.temp_file "mehari_cgi" ".txt" in
  let* fd = Lwt_unix.openfile temp_path [ O_RDWR ] 0o666 in
  Lwt.finalize
    (fun () ->
      match%lwt
        Lwt_process.exec (path, [||])
          ~stdout:(`FD_copy (Lwt_unix.unix_file_descr fd))
      with
      | WEXITED 0 -> (
          let stream = Lwt_io.of_fd fd ~mode:Input |> Lwt_io.read_lines in
          match%lwt Lwt_stream.get stream with
          | None -> Lwt.return_none
          | Some line ->
              if Bytes.(of_string line |> length) > 1024 then Lwt.return_none
              else
                let$ grp = Re.exec_opt meta line in
                let$ code = int_of_string_opt (Re.Group.get grp 1) in
                Lwt.return_some (code, Re.Group.get grp 2, stream))
      | _ -> Lwt.return_none)
    (fun () -> Lwt_unix.close fd)

let run_cgi path =
  match%lwt cgi_output path with
  | None -> Mehari.respond Mehari.cgi_error ""
  | Some (code, meta, body) ->
      let* lines = Lwt_stream.to_list body in
      let body = String.concat "\n" lines in
      Mehari.raw_respond code ~meta ~body
