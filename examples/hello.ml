open Lwt.Syntax

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      Mehari_lwt_unix.run_lwt ~certs:(Single cert) (fun _ ->
          Mehari_lwt_unix.respond_text "Hello")
    end
