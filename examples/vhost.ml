open Mehari

let respond_text msg _ _ =
  Format.kasprintf Response.text "Requesting subdomain %s" msg

let router =
  Mehari_eio.virtual_host
    [
      Mehari_eio.domain "foo.localhost" (respond_text "foo")
        ~all:(respond_text "*.foo");
      Mehari_eio.domain "bar.localhost"
        (respond_text "Requesting subdomain bar");
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
