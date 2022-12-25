let router =
  Mehari_eio.virtual_hosts
    ~default:(fun _ -> Mehari.response_text "default")
    [
      ("localhost.foo", fun _ -> Mehari.response_text "foo");
      ("localhost.bar", fun _ -> Mehari.response_text "foo");
    ]

let main ~net ~cwd =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    router

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
