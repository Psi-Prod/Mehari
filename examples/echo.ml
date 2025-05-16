open Mehari
open Lwt.Syntax
module M = Mehari_lwt_unix

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      M.router
        [
          M.route ~regex:true "/echo/(.*)" (fun req ->
              Request.param req 1 |> M.respond_text);
        ]
      |> M.logger |> M.run ~certs:(Single cert)
    end
