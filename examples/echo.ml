module M = Mehari_lwt_unix
open Lwt.Syntax

let main () =
  let* certchains = Common.Lwt.load_certchains () in
  M.router
    [
      M.route ~regex:true "/echo/(.*)" (fun req ->
          Mehari.param req 1 |> M.respond_text);
    ]
  |> M.logger |> M.run_lwt ~certchains

let () = Lwt_main.run (main ())
