module Mehari = Mehari_unix

let () =
  Mehari.run (fun req ->
      match Mehari.uri req |> Uri.verbatim_query with
      | None -> Mehari.respond_text ""
      | Some msg -> Mehari.respond_text msg)
