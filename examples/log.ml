module M_unix = Mehari_lwt_unix

let n = ref 0

let () =
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  M_unix.router
    [
      M_unix.route "/" (fun _ ->
          incr n;
          M_unix.info (fun log -> log "Request n°: %i" !n);
          M_unix.respond_text "This request is logged");
    ]
  |> M_unix.logger |> M_unix.run
