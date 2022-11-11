let () =
  let open Mehari in
  run (fun _ -> respond (success (text "Hello world")) empty_mime)
