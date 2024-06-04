module M = Mehari_lwt_unix
open Lwt.Syntax

let main () =
  let* certchains = Common.Lwt.load_certchains () in
  M.router
    [ M.route "/cgi" (fun req -> M.run_cgi "./examples/cgi_script.py" req) ]
  |> M.run_lwt ~certchains

let () = Lwt_main.run (main ())
