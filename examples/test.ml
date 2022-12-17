let main ~net ~cwd =
  let ( / ) = Eio.Path.( / ) in
  let certchains = [ (cwd / "cert.pem", cwd / "key.pem") ] in
  Mehari_eio.run ~certchains net (fun _ -> Mehari.response_text "ok")

let () =
  Eio_main.run (fun env ->
      Mirage_crypto_rng_eio.run
        (module Mirage_crypto_rng.Fortuna)
        env
        (fun () -> main ~net:(Eio.Stdenv.net env) ~cwd:(Eio.Stdenv.cwd env)))
