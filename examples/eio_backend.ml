let main ~net ~cwd =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    (fun _ -> Mehari.response_text "Hello from an effect based server!")

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:(Eio.Stdenv.net env) ~cwd:(Eio.Stdenv.cwd env)
