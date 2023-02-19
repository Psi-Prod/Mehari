let router cwd =
  Mehari_eio.router
    [
      Mehari_eio.route "/" (fun _ ->
          Mehari_eio.response_document Eio.Path.(cwd / "README.md"));
      Mehari_eio.route ~regex:true "/sources/(.*)" (Mehari_eio.static cwd);
    ]

let main ~net ~cwd =
  let certchains =
    Eio.Path.
      [
        X509_eio.private_of_pems ~cert:(cwd / "cert.pem")
          ~priv_key:(cwd / "key.pem");
      ]
  in
  Mehari_eio.run net ~certchains (router cwd)

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
