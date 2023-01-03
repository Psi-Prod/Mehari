module M_unix = Mehari_lwt_unix

let () =
  M_unix.router
    [
      M_unix.route "/cgi" (fun req ->
          Mehari_lwt_unix.run_cgi "./examples/cgi_script.py" req);
    ]
  |> M_unix.run
