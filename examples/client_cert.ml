let router =
  Mehari_eio.router
    [
      Mehari_eio.route "/" (fun req ->
          match Mehari.clientcert req with
          | [] -> Mehari.(response client_cert_req) "Certificate plz"
          | hd :: _ ->
              X509.Certificate.encode_pem hd |>
              Cstruct.to_string |>
              Printf.sprintf "Client certificate ~nyoron\n%s" |>
              Mehari.response_text);
    ]

let main ~net ~cwd =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    router

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~net:env#net ~cwd:env#cwd
