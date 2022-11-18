let () =
  Mehari.router
    [
      Mehari.route "/echo/:text" (fun req ->
          Mehari.param req "text" |> Mehari.respond_text);
    ]
  |> Mehari.run
