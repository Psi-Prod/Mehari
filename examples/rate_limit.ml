module M = Mehari_lwt_unix
open Lwt.Syntax

let low_limit = M.make_rate_limit 5 `Minute
let high_limit = M.make_rate_limit ~period:10 2 `Second

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      M.router
        [
          M.route "/low" ~rate_limit:low_limit (fun _ ->
              M.respond_text "5 requests per minute authorized");
          M.route "/high" ~rate_limit:high_limit (fun _ ->
              M.respond_text "2 requests per 10 seconds authorized");
        ]
      |> M.run ~certs:(Single cert)
    end
