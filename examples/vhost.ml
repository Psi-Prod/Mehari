open Mehari

let router =
  Mehari_eio.virtual_hosts
    [
      ("localhost.foo", fun _ _ -> Response.text "Requesting subdomain foo");
      ("localhost.bar", fun _ _ -> Response.text "Requesting subdomain bar");
    ]

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let cert =
    let ( / ) = Eio.Path.( / ) in
    X509_eio.private_of_pems ~cert:(env#cwd / "cert.pem")
      ~priv_key:(env#cwd / "key.pem")
  in
  Mehari_eio.run ~certs:(Single cert) router env
