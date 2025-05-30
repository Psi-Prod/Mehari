open Mehari
open Lwt.Syntax
module M = Mehari_lwt_unix

let any = Path.variable ~from_string:Option.some ~to_string:Fun.id

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      M.router
        [
          M.route
            Path.(~/"sources" /: any)
            (fun target _req -> M.respond_text target);
        ]
      |> M.logger |> M.run ~certs:(Single cert)
    end
