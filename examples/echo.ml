let () =
  Mehari_unix.router
    [
      Mehari_unix.route "/echo/:text" (fun req ->
          Mehari.param req "text" |> Mehari.respond_text);
    ]
  |> Mehari_unix.logger |> Mehari_unix.run
