let () =
  Mehari_lwt_unix.set_log_lvl Warning;
  Logs.set_level ~all:true (Some Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Mehari_lwt_unix.router
    [
      Mehari_lwt_unix.route ~typ:`Regex "/(.*)" (fun req ->
          Mehari_lwt_unix.static "." req);
    ]
  |> Mehari_lwt_unix.logger |> Mehari_lwt_unix.run
