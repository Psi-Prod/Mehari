let counter = ref 0

let inc_count handler req =
  incr counter;
  handler req

let () =
  let open Mehari in
  router
    [
      route "/" (fun _ ->
          respond_gemtext
            Gemtext.
              [
                link "/incr" ~name:"Increment counter";
                text (Printf.sprintf "Counter = %i" !counter);
              ]);
      route "/incr" ~mw:inc_count (fun _ -> respond redirect_temp "/");
    ]
  |> run
