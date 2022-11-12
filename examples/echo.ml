let () =
  let open Mehari in
  run (fun req ->
      match uri req |> Uri.verbatim_query with
      | None -> respond_text ""
      | Some msg -> respond_text msg)
