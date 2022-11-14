let rate_limit = Mehari.make_rate_limit ~period:10 2 `Second

let () =
  Mehari.router
    [ Mehari.route "/limited" (fun _ -> Mehari.respond_text "Not limited") ]
  |> Mehari.run
