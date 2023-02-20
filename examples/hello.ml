open Lwt.Infix

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
  >>= fun certchain ->
  (fun _ -> Mehari_lwt_unix.respond_text "Hello")
  |> Mehari_lwt_unix.run_lwt ~certchains:[ certchain ]

let () = Lwt_main.run (main ())
