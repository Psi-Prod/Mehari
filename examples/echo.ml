let () =
  let open Mehari in
  run (fun req ->
      uri req |> Uri.to_string |> Printf.sprintf "Url: %s" |> respond input)
