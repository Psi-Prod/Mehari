module Mehari_io = Mehari_lwt_unix

let () =
  Mehari_io.router
    [
      Mehari_io.route "/cgi" (fun req ->
          Mehari_io.run_cgi "./examples/cgi_script.py" req);
    ]
  |> Mehari_io.run
