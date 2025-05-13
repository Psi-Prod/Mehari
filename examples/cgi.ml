module M = Mehari_lwt_unix
open Lwt.Syntax

let router =
  M.router
    [ M.route "/cgi" (fun req -> M.run_cgi "./examples/cgi_script.py" req) ]

let main () =
  let* cert = X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_lwt_unix.run_lwt ~certchains:[ cert ] router

let () = Lwt_main.run (main ())
