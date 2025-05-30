module M = Mehari_lwt_unix
open Mehari
open Lwt.Syntax

let router =
  M.router
    [
      M.route
        Path.(~/"cgi")
        (fun req -> M.run_cgi "./examples/cgi_script.py" req);
    ]

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      Mehari_lwt_unix.run ~certs:(Single cert) router
    end
