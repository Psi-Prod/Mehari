let () =
  Mehari.router [ Mehari.route "/" (fun _ -> Mehari.directory_listing "./") ]
  |> Mehari.run
