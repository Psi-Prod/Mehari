module Mehari_io = Mehari_lwt_unix

let n = ref 0

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Mehari_io.router
    [
      Mehari_io.route "/" (fun _ ->
          incr n;
          Mehari_io.info (fun log -> log "Request n°: %i" !n);
          Mehari_io.respond_text "This request is logged");
    ]
  |> Mehari_io.logger |> Mehari_io.run
