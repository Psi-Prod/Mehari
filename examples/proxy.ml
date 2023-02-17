(*
echo -e "gemini://foo/" | openssl s_client -crlf -connect localhost:1965 -servername foo -ign_eof
*)

let router =
  Mehari_eio.virtual_hosts ~meth:`ByURL
    [
      ("foo", fun _ -> Mehari.response_text "foo");
      ("bar", fun _ -> Mehari.response_text "bar");
    ]

let main ~net ~cwd =
  let certchains =
    Eio.Path.
      [
        X509_eio.private_of_pems ~cert:(cwd / "cert.pem")
          ~priv_key:(cwd / "key.pem");
      ]
  in
  Mehari_eio.run net ~certchains ~verify_url_host:false router

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
