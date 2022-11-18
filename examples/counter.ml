module MIO = Mehari_unix

let counter = ref 0

let inc_count handler req =
  incr counter;
  handler req

let () =
  let open Mehari in
  MIO.router
    [
      MIO.route "/" (fun _ ->
          respond_gemtext
            Gemtext.
              [
                link "/incr" ~name:"Increment counter";
                text (Printf.sprintf "Counter = %i" !counter);
              ]);
      MIO.route "/incr" ~mw:inc_count (fun _ -> respond redirect_temp "/");
    ]
  |> MIO.run
