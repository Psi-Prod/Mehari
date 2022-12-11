module MIO = Mehari_unix

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let () =
  MIO.router
    [
      MIO.route "/" (fun _ ->
          Mehari.respond_gemtext
            Mehari.Gemtext.
              [
                link "/incr" ~name:"Increment counter";
                text (Printf.sprintf "Counter = %i" !counter);
              ]);
      MIO.route "/incr" ~mw:incr_count
        Mehari.(fun _ -> respond redirect_temp "/");
    ]
  |> MIO.run
