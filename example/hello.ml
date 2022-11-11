let () =
  Mehari.run (function
    | "gemini://localhost/foo\r\n" -> "20 text/gemini\r\nHello, world!"
    | _ -> "20 text/gemini\r\nNique ta race")
