module M = Mehari_lwt_unix
open Lwt.Syntax

let n = ref 0

let () =
  M.set_log_lvl Info;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let main () =
  let* certchains = Common.Lwt.load_certchains () in
  M.router
    [
      M.route "/" (fun _ ->
          incr n;
          M.info (fun log -> log "Request n°: %i" !n);
          M.respond_text "This request is logged");
    ]
  |> M.logger |> M.run_lwt ~certchains

let () = Lwt_main.run (main ())
