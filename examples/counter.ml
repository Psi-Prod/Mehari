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
            [
              Gemtext.Link { url = "/"; name = Some "Increment counter" };
              Gemtext.Text (Printf.sprintf "Counter = %i" !counter);
            ]);
      route "/inc" ~mw:inc_count (fun _ -> respond redirect_temp "/");
    ]
  |> run
