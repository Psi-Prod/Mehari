let low_limit = Mehari.make_rate_limit 5 `Minute
let hight_limit = Mehari.make_rate_limit ~period:10 2 `Second

let () =
  Mehari.router
    [
      Mehari.route "/low" ~rate_limit:low_limit (fun _ ->
          Mehari.respond_text "5 requests per minute authorized");
      Mehari.route "/hight" ~rate_limit:hight_limit (fun _ ->
          Mehari.respond_text "2 requests per 10 seconds authorized");
    ]
  |> Mehari.run
