open Mehari
module Mehari_io = Mehari_lwt_unix

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let () =
  Mehari_io.router
    [
      Mehari_io.route "/" (fun _ ->
          Mehari_io.respond_gemtext
            Gemtext.
              [
                link "/incr" ~name:"Increment counter";
                text (Printf.sprintf "Counter = %i" !counter);
              ]);
      Mehari_io.route "/incr" ~mw:incr_count (fun _ ->
          Mehari_io.respond Mehari.redirect_temp "/");
    ]
  |> Mehari_io.run
