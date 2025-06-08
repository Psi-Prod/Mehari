open Mehari
open Eio

let src = Logs.Src.create "mehari.eio.cgi"

module Log = (val Logs.src_log src)

external reraise : exn -> 'a = "%reraise"

let cgi_err = Response.respond Status.cgi_error ""

let make_cgi_env req ~script_path =
  let open Mehari.Private.Cgi in
  make req ~script_path |> to_env
  |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)

let run_process ~script_path req mgr =
  Switch.run ~name:"Mehari_eio.Cgi.run_process" @@ fun sw ->
  let r, w = Process.pipe mgr ~sw in
  let env = make_cgi_env req ~script_path in
  let proc = Process.spawn ~sw mgr ~env ~stdout:w [ script_path ] in
  Flow.close w;
  let output = Buf_read.(parse_exn take_all) r ~max_size:max_int in
  Flow.close r;
  let () =
    match Process.await proc with
    | `Exited 0 -> ()
    | status ->
        Log.warn (fun log ->
            log "CGI script terminates with an abnormal exit status: %a"
              Process.pp_status status)
  in
  output

let run_cgi ?(timeout = 5.0) ?(non_parsed = false) script_path req env =
  try
    match
      Time.with_timeout env#clock timeout (fun () ->
          if non_parsed then
            let output = run_process ~script_path req env#process_mgr in
            Ok (Response.Private.unsafe_raw output)
          else failwith "not implemented")
    with
    | Ok resp -> resp
    | Error `Timeout -> cgi_err
  with exn ->
    Log.err (fun log ->
        log "Exception occured during CGI script running: %S"
          (Printexc.to_string exn));
    reraise exn
