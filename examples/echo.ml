module M_unix = Mehari_unix

let () =
  M_unix.router
    [
      M_unix.route ~typ:`Regex "/echo/(.*)" (fun req ->
          Mehari.param req 1 |> M_unix.respond_text);
    ]
  |> M_unix.logger |> M_unix.run
