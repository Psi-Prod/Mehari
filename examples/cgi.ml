module M = Mehari_lwt_unix
open Lwt.Infix

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" >>= fun cert ->
  M.router
    [ M.route "/cgi" (fun req -> M.run_cgi "./examples/cgi_script.py" req) ]
  |> M.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
