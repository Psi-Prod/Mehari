open Lwt.Syntax

let main () =
  let* certchains = Common.Lwt.load_certchains () in
  (fun _ -> Mehari_lwt_unix.respond_text "Hello")
  |> Mehari_lwt_unix.run_lwt ~certchains

let () = Lwt_main.run (main ())
