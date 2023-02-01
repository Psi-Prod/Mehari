module Mehari_io = Mehari_lwt_unix
open Lwt.Infix

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" >>= fun cert ->
  Mehari_io.router
    [
      Mehari_io.route "/cgi" (fun req ->
          Mehari_io.run_cgi "./examples/cgi_script.py" req);
    ]
  |> Mehari_io.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
