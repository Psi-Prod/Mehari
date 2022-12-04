let () =
  Mehari_unix.router
    [
      Mehari_unix.route ~typ:`Regex "/echo/(.*)" (fun req ->
          Mehari.param req 1 |> Mehari.respond_text);
    ]
  |> Mehari_unix.run
