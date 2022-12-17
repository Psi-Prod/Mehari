let router req =
  match Mehari.query req with
  | None -> Mehari.(response input "Enter a number")
  | Some number -> (
      match int_of_string_opt number with
      | None -> Mehari.(response bad_request) "Enter a valid number!"
      | Some n ->
          let body = Seq.init n (Printf.sprintf "%i\n") |> Mehari.seq in
          Mehari.(response_body body (text_mime "plain")))

let main ~net ~cwd =
  let ( / ) = Eio.Path.( / ) in
  let certchains = [ (cwd / "cert.pem", cwd / "key.pem") ] in
  Mehari_eio.run ~certchains net router

let () =
  Eio_main.run (fun env ->
      Mirage_crypto_rng_eio.run
        (module Mirage_crypto_rng.Fortuna)
        env
        (fun () -> main ~net:(Eio.Stdenv.net env) ~cwd:(Eio.Stdenv.cwd env)))
