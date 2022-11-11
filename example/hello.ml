let () =
  let open Mehari in
  run (fun _ -> respond (success (text "Hello world")) Mehari.empty_mime)
