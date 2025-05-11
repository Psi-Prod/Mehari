module Mehari_io = Mehari_lwt_unix
open Mehari
open Lwt.Syntax

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let main () =
  let* certchains = Common.Lwt.load_certchains () in
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
  |> Mehari_io.run_lwt ~certchains

let () = Lwt_main.run (main ())
