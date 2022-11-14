let () =
  Mehari.router
    [
      Mehari.route "/hello" (fun _ -> Mehari.respond_text "Hello");
      Mehari.route "/world" (fun _ -> Mehari.respond_text "World");
    ]
  |> Mehari.run
