let src = Logs.Src.create "mehari.log"

module Log = (val Logs.src_log src)

let now () = Mirage_ptime.now () |> Ptime.to_float_s

let iter_backtrace f backtrace =
  String.split_on_char '\n' backtrace
  |> List.iter (function "" -> () | l -> f l)

external reraise : exn -> 'a = "%reraise"

let logger handler req =
  let start = now () in
  try
    let response = handler req in
    Log.info (fun log ->
        log "Serve '%s' %a"
          (Request.uri req |> Uri.path_and_query)
          Ipaddr.pp (Request.ip req));
    let code = Response.status response in
    let elapsed = now () -. start in
    Log.info (fun log -> log "%i in %f µs" code (elapsed *. 1e6));
    response
  with exn ->
    let backtrace = Printexc.get_backtrace () in
    Log.warn (fun log -> log "Aborted by: %s" (Printexc.to_string exn));
    iter_backtrace (fun line -> Log.warn (fun log -> log "%s" line)) backtrace;
    reraise exn
