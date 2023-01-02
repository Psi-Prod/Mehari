let router cwd =
  Mehari_eio.router
    [
      Mehari_eio.route "/" (fun _ ->
          Mehari_eio.response_document Eio.Path.(cwd / "README.md"));
      Mehari_eio.route ~typ:`Regex "/examples/(.*)"
        (Eio.Path.(cwd / "examples") |> Mehari_eio.static);
    ]

let main ~net ~cwd =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    (router cwd)

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
