let () =
  let open Mehari in
  router
    [
      route "/hello" (fun _ -> respond_text "Hello");
      route "/world" (fun _ -> respond_text "World");
    ]
  |> run
