module Mehari_io = Mehari_lwt_unix
open Lwt.Infix

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let main () =
  X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" >>= fun cert ->
  Mehari_io.router
    [
      Mehari_io.route "/" (fun _ ->
          let open Mehari.Gemtext in
          Mehari_io.respond_gemtext
            [
              link "/incr" ~name:"Increment counter";
              text (Printf.sprintf "Counter = %i" !counter);
            ]);
      Mehari_io.route "/incr" ~mw:incr_count (fun _ ->
          Mehari_io.respond Mehari.redirect_temp "/");
    ]
  |> Mehari_io.run_lwt ~certchains:[ cert ]

let () = Lwt_main.run (main ())
