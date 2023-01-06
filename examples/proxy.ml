(*
echo -e "gemini://foo/" | openssl s_client -crlf -connect localhost:1965 -servername foo -ign_eof
*)

let router =
  Mehari_eio.virtual_hosts
    [
      ("foo", fun _ -> Mehari.response_text "foo");
      ("bar", fun _ -> Mehari.response_text "bar");
    ]

let main ~net ~cwd =
  let certchains = Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ] in
  Mehari_eio.run net ~certchains ~verifyurlhost:false router

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
