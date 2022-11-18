module MIO = Mehari_unix

let () =
  MIO.router
    [
      MIO.route "/hello" (fun _ -> Mehari.respond_text "Hello");
      MIO.route "/world" (fun _ -> Mehari.respond_text "World");
    ]
  |> MIO.run
