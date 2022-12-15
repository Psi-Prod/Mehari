open Mehari
open Lwt.Syntax

let make_env req path =
  let empty = {|""|} in
  [|
    ("AUTH_TYPE", "CERTIFICATE");
    ("CONTENT_LENGTH", empty);
    ("CONTENT_TYPE", empty);
    ("GATEWAY_INTERFACE", "CGI/1.1");
    ("PATH_INFO", uri req |> Uri.path |> Uri.pct_decode);
    ("PATH_TRANSLATED", path);
    ("QUERY_STRING", query req |> Option.value ~default:"");
    ("REMOTE_ADDR", ip req |> Ipaddr.to_string);
    ("REMOTE_HOST", uri req |> Uri.host |> Option.value ~default:"");
    ("REMOTE_IDENT", empty);
    ("REQUEST_METHOD", empty);
    ("SCRIPT_NAME", Filename.concat (Unix.getcwd ()) path);
    ("SERVER_NAME", uri req |> Uri.host |> Option.value ~default:"");
    ("SERVER_PORT", port req |> Int.to_string);
    ("SERVER_PROTOCOL", "GEMINI");
    ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
  |]
  |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)

let chunk_size = 16384

exception Exited

let read_body (proc : Lwt_process.process_in) =
  Lwt_seq.unfold_lwt
    (fun finished ->
      if finished then Lwt.return_none
      else
        let* data = Lwt_io.read ~count:chunk_size proc#stdout in
        if String.length data = chunk_size then Lwt.return_some (data, true)
        else
          match proc#state with
          | Running -> Lwt.return_some (data, true)
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

let run_cgi ?(timeout = 5.0) ?(nph = false) path req =
  try%lwt
    with_proc ~timeout ~env:(make_env req path) (path, [||]) (fun proc ->
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
