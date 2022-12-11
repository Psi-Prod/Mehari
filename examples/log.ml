module Logs_reporter = Mirage_logs.Make (Pclock)

let setup_logs f =
  Logs.set_level (Some Info);
  Logs_reporter.(create () |> run) f

let n = ref 0

let () =
  setup_logs (fun () ->
      (fun _ ->
        incr n;
        Mehari_unix.info (fun log -> log "Request n°: %i" !n);
        Mehari.respond_text "Success")
      |> Mehari_unix.logger |> Mehari_unix.run_lwt)
  |> Lwt_main.run
