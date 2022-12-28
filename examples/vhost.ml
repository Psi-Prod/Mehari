let router =
  Mehari_eio.virtual_hosts
    [
      ("localhost.foo", fun _ -> Mehari.response_text "foo");
      ("localhost.bar", fun _ -> Mehari.response_text "foo");
    ]

let main ~net ~cwd =
  let certchains =
    Eio.Path.
      [
        (cwd / "cert_foo.pem", cwd / "key_foo.pem");
        (cwd / "cert_bar.pem", cwd / "key_bar.pem");
      ]
  in
  Mehari_eio.run net ~certchains router

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
