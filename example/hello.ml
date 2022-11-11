let () =
  let open Mehari in
  router
    [
      route "/hello" (fun _ -> respond (success (text "Hello")) empty);
      route "/world" (fun _ -> respond (success (text "World")) empty);
    ]
  |> run
