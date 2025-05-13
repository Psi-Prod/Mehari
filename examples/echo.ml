open Mehari
open Lwt.Syntax
module M = Mehari_lwt_unix

let main () =
  let* cert = X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" in
  M.router
    [
      M.route ~regex:true "/echo/(.*)" (fun req ->
          Request.param req 1 |> M.respond_text);
    ]
  |> M.logger
  |> M.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
