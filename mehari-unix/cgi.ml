open Lwt.Syntax

(* TODO env *)
(* TODO timeout *)

let make_env _req = Array.append [||] [| ("GATEWAY_INTERFACE", "CGI/1.1") |]
let chunk_size = 16384

exception Exited

let read_body (proc : Lwt_process.process_in) =
  Lwt_stream.from (fun () ->
      let* data = Lwt_io.read ~count:chunk_size proc#stdout in
      if String.length data = chunk_size then Lwt.return_some data
      else
        match proc#state with
        | Running -> Lwt.return_some data
        | Exited (WEXITED 0) -> Lwt.return_none
        | _ -> Lwt.fail Exited)

let meta = Re.compile Re.(seq [ group (rep1 digit); space; group (rep any) ])
let ( let$ ) opt f = match opt with None -> Lwt.return_none | Some x -> f x

let parse_header in_chan =
  match%lwt Lwt_io.read_line_opt in_chan with
  | None -> Lwt.return_none
  | Some header ->
      if Bytes.(of_string header |> length) > 1024 then Lwt.return_none
      else
        let$ grp = Re.exec_opt meta header in
        let$ code = int_of_string_opt (Re.Group.get grp 1) in
        Lwt.return_some (code, Re.Group.get grp 2)

let with_proc cmd f =
  let proc = Lwt_process.open_process_in cmd in
  Lwt.finalize
    (fun () -> f proc)
    (fun () ->
      let* _ = proc#close in
      Lwt.return_unit)

let run_cgi ?(nph = false) path =
  with_proc (path, [||]) (fun proc ->
      (* TODO: Better stream handler. *)
      try%lwt
        if nph then
          let* chunks = read_body proc |> Lwt_stream.to_list in
          `Body (String.concat "" chunks) |> Mehari.raw_respond
        else
          match%lwt parse_header proc#stdout with
          | None -> Mehari.respond Mehari.cgi_error ""
          | Some (code, meta) ->
              let* chunks = read_body proc |> Lwt_stream.to_list in
              Mehari.raw_respond (`Full (code, meta, String.concat "" chunks))
      with Exited -> Mehari.respond Mehari.cgi_error "")
