module M_unix = Mehari_lwt_unix

let low_limit = M_unix.make_rate_limit 5 `Minute
let hight_limit = M_unix.make_rate_limit ~period:10 2 `Second

let () =
  M_unix.router
    [
      M_unix.route "/low" ~rate_limit:low_limit (fun _ ->
          M_unix.respond_text "5 requests per minute authorized");
      M_unix.route "/hight" ~rate_limit:hight_limit (fun _ ->
        M_unix.respond_text "2 requests per 10 seconds authorized");
    ]
  |> M_unix.run
