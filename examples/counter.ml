open Mehari
module M_unix = Mehari_unix

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let () =
  M_unix.router
    [
      M_unix.route "/" (fun _ ->
          M_unix.respond_gemtext
            Gemtext.
              [
                link "/incr" ~name:"Increment counter";
                text (Printf.sprintf "Counter = %i" !counter);
              ]);
      M_unix.route "/incr" ~mw:incr_count (fun _ ->
          Mehari_unix.respond Mehari.redirect_temp "/");
    ]
  |> M_unix.run
