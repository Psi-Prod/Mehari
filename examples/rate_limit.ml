let rate_limit = Mehari.make_rate_limit ~period:10 2 `Second

let () =
  Mehari.router
    [
      Mehari.route "/limited" ~mw:rate_limit (fun _ ->
          Mehari.respond_text "Not limited");
    ]
  |> Mehari.run
