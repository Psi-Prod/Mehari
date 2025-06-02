open Mehari

let src = Logs.Src.create "mehari.miou-unix.cgi"

module Log = (val Logs.src_log src)

let cgi_err = Response.respond Status.cgi_error ""

let pp_process_status ppf = function
  | Unix.WEXITED i -> Format.fprintf ppf "Exited (code %i)" i
  | WSIGNALED i -> Format.fprintf ppf "Exited (signal %i)" i
  | WSTOPPED i -> Format.fprintf ppf "Stopped (signal %i)" i

let make_cgi_env req ~script_path =
  let open Private.Cgi in
  make req ~script_path |> to_env
  |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)

let run_process path env =
  let temp_fname = Filename.temp_file "mehari-cgi-script" "output" in
  let stdout = Unix.openfile temp_fname [ Unix.O_WRONLY ] 0o600 in
  let pid =
    Unix.create_process_env path [||] env Unix.stdin stdout Unix.stderr
  in
  let c = Miou.Computation.create () in
  let rec handler _sigchld =
    match Unix.waitpid [ WNOHANG ] pid with
    | 0, _ -> ignore (Miou.sys_signal Sys.sigchld (Sys.Signal_handle handler))
    | pid', status ->
        assert (pid = pid');
        assert (Miou.Computation.try_return c status)
  in
  ignore (Miou.sys_signal Sys.sigchld (Sys.Signal_handle handler));
  match Miou.Computation.await_exn c with
  | Unix.WEXITED 0 ->
      let fd = Miou_unix.of_file_descr stdout in
      let output = File.read_chunks fd |> List.of_seq |> String.concat "" in
      Miou_unix.close fd;
      Some output
  | status ->
      Logs.warn (fun log ->
          log "CGI script terminates with an abnormal exit status: %a"
            pp_process_status status);
      None

let with_timeout timeout task =
  let exception Timeout in
  let timeout =
    Miou.async (fun () ->
        Miou_unix.sleep timeout;
        raise_notrace Timeout)
  in
  match Miou.await_first [ timeout; Miou.async task ] with
  | Ok resp -> resp
  | Error Timeout -> cgi_err
  | Error exn -> Miou.reraise exn

let run_cgi ?(timeout = 5.0) ?(non_parsed = false) script_path req =
  try
    with_timeout timeout (fun () ->
        if non_parsed then
          let env = make_cgi_env req ~script_path in
          match run_process script_path env with
          | None -> cgi_err
          | Some output -> Response.Private.unsafe_raw output
        else failwith "not implemented")
  with exn ->
    Logs.err (fun log ->
        log "Exception occured during CGI script running: %S"
          (Printexc.to_string exn));
    Miou.reraise exn
