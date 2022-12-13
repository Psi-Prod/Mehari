module MIO = Mehari_unix

let low_limit = MIO.make_rate_limit 5 `Minute
let hight_limit = MIO.make_rate_limit ~period:10 2 `Second

let () =
  MIO.router
    [
      MIO.route "/low" ~rate_limit:low_limit (fun _ ->
          Mehari_unix.respond_text "5 requests per minute authorized");
      MIO.route "/hight" ~rate_limit:hight_limit (fun _ ->
          Mehari_unix.respond_text "2 requests per 10 seconds authorized");
    ]
  |> MIO.run
