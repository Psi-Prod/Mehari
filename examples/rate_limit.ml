module Mehari_io = Mehari_lwt_unix
open Lwt.Infix

let low_limit = Mehari_io.make_rate_limit 5 `Minute
let high_limit = Mehari_io.make_rate_limit ~period:10 2 `Second

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" >>= fun cert ->
  Mehari_io.router
    [
      Mehari_io.route "/low" ~rate_limit:low_limit (fun _ ->
          Mehari_io.respond_text "5 requests per minute authorized");
      Mehari_io.route "/high" ~rate_limit:high_limit (fun _ ->
          Mehari_io.respond_text "2 requests per 10 seconds authorized");
    ]
  |> Mehari_io.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
