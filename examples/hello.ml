open Lwt.Syntax

let main () =
  let* cert = X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_lwt_unix.run_lwt ~certchains:[ cert ] (fun _ ->
      Mehari_lwt_unix.respond_text "Hello")

let () = Lwt_main.run (main ())
