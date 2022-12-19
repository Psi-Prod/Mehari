let router cwd =
  Mehari_eio.router
    [
      Mehari_eio.route "/readme" (fun _ ->
          Mehari_eio.response_document Mehari.plaintext
            Eio.Path.(cwd / "README.md"));
    ]

let main ~net ~cwd =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    (router cwd)

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
