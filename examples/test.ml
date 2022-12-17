let main ~net ~cwd =
  let ( / ) = Eio.Path.( / ) in
  Mehari_eio.Server.run
    ~certchains:[ (cwd / "cert.pem", cwd / "key.pem") ]
    net
    (fun _ -> Mehari.response_text "ok")

let () =
  Eio_main.run (fun env ->
      Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env
      @@ fun () -> main ~net:(Eio.Stdenv.net env) ~cwd:(Eio.Stdenv.cwd env))
