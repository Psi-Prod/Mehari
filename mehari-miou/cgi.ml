open Mehari

let src = Logs.Src.create "mehari-miou.cgi"

module Log = (val Logs.src_log src)

let cgi_err = Response.respond Status.cgi_error ""

let pp_process_status ppf = function
  | Unix.WEXITED i -> Format.fprintf ppf "Exited (code %i)" i
  | WSIGNALED i -> Format.fprintf ppf "Exited (signal %i)" i
  | WSTOPPED i -> Format.fprintf ppf "Stopped (signal %i)" i

let make_cgi_env req ~script_path =
  Cgi.make req ~script_path |> Cgi.to_env
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
      let buf = Bytes.create 0x7ff in
      let pull fd =
        match Miou_unix.read fd buf with
        | 0 -> None
        | len -> Some (Bytes.sub_string buf 0 len)
      in
      let chunks = Flux.Source.resource ~finally:Miou_unix.close pull fd in
      let output, leftover =
        Flux.(Stream.run ~from:chunks ~via:Flow.identity ~into:Sink.string)
      in
      Option.iter Flux.Source.dispose leftover;
      Some output
  | status ->
      Log.warn (fun log ->
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
    Log.err (fun log ->
        log "Exception occured during CGI script running: %S"
          (Printexc.to_string exn));
    Miou.reraise exn
