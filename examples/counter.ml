module Mehari_io = Mehari_lwt_unix
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
          let open Mehari.Gemtext in
          Mehari_io.respond_gemtext
            [
              link "/incr" ~name:"Increment counter";
              text (Printf.sprintf "Counter = %i" !counter);
            ]);
      Mehari_io.route "/incr" ~mw:incr_count (fun _ ->
          Mehari_io.respond Mehari.redirect_temp "/");
    ]
  |> Mehari_io.run_lwt ~certchains

let () = Lwt_main.run (main ())
