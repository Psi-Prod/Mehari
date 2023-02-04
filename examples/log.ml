module Mehari_io = Mehari_lwt_unix
open Lwt.Infix

let n = ref 0

let () =
  Mehari_io.set_log_lvl Info;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" >>= fun cert ->
  Mehari_io.router
    [
      Mehari_io.route "/" (fun _ ->
          incr n;
          Mehari_io.info (fun log -> log "Request n°: %i" !n);
          Mehari_io.respond_text "This request is logged");
    ]
  |> Mehari_io.logger
  |> Mehari_io.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
