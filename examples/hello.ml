let () =
  (fun _ -> Mehari.respond_text "Hello") |> Mehari_unix.run ~v4:"127.0.0.1/8"
