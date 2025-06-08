open Mehari
open Lwt.Syntax

let n = ref 0

let () =
  Logs.Src.set_level Mehari.log_src (Some Info);
  Logs.Src.set_level Mehari_lwt_unix.log_src (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      Mehari_lwt_unix.router
        [
          Mehari_lwt_unix.route Path.root (fun _ ->
              incr n;
              Logs.info (fun log -> log "Request n°: %i" !n);
              Mehari_lwt_unix.respond_text "This request is logged");
        ]
      |> Mehari_lwt_unix.logger
      |> Mehari_lwt_unix.run ~certs:(Single cert)
    end
