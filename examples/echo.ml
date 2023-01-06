module Mehari_io = Mehari_lwt_unix

let () =
  Mehari_io.router
    [
      Mehari_io.route ~typ:`Regex "/echo/(.*)" (fun req ->
          Mehari.param req 1 |> Mehari_io.respond_text);
    ]
  |> Mehari_io.logger |> Mehari_io.run
