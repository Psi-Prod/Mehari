module Mehari_io = Mehari_lwt_unix

let low_limit = Mehari_io.make_rate_limit 5 `Minute
let hight_limit = Mehari_io.make_rate_limit ~period:10 2 `Second

let () =
  Mehari_io.router
    [
      Mehari_io.route "/low" ~rate_limit:low_limit (fun _ ->
          Mehari_io.respond_text "5 requests per minute authorized");
      Mehari_io.route "/hight" ~rate_limit:hight_limit (fun _ ->
          Mehari_io.respond_text "2 requests per 10 seconds authorized");
    ]
  |> Mehari_io.run
