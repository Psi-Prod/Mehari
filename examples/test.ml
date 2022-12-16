let main ~net ~cwd =
  let ( / ) = Eio.Path.( / ) in
  Mehari_eio.Server.run
    ~certchains:[ (cwd / "cert.pem", cwd / "key.pem") ]
    net
    (fun _ -> Mehari.response_text "ok")

let () =
  Mirage_crypto_rng_unix.initialize ();
  Eio_main.run (fun env ->
      main ~net:(Eio.Stdenv.net env) ~cwd:(Eio.Stdenv.cwd env))
