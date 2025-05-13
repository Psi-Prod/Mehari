module Mehari_io = Mehari_lwt_unix
open Mehari
open Lwt.Syntax

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      Mehari_io.router
        [
          Mehari_io.route "/" (fun _ ->
              Mehari_io.respond_gemtext
                [
                  Gemtext.link "/incr" ~name:"Increment counter";
                  Gemtext.text (Printf.sprintf "Counter = %i" !counter);
                ]);
          Mehari_io.route "/incr" ~mw:incr_count (fun _ ->
              Mehari_io.respond Status.redirect_temp "/");
        ]
      |> Mehari_io.run_lwt ~certs:(Single cert)
    end
