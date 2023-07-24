module M = Mehari_lwt_unix
open Lwt.Infix

let n = ref 0

let () =
  M.set_log_lvl Info;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" >>= fun cert ->
  M.router
    [
      M.route "/" (fun _ ->
          incr n;
          M.info (fun log -> log "Request n°: %i" !n);
          M.respond_text "This request is logged");
    ]
  |> M.logger
  |> M.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
