(** To generate correct certificates to test this example, run:

    {@bash[
      openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256 -days 365 -nodes
        --subj "/CN=localhost" -addext "subjectAltName = DNS:foo.localhost, DNS:bar.localhost"
    ]} *)

open Mehari

let respond_msg msg _ _ =
  Format.kasprintf Response.text "Requesting subdomain %s" msg

let router =
  Mehari_eio.virtual_host
    [
      Mehari_eio.domain "localhost" (fun _ _ ->
          Response.text "No subdomain requested");
      Mehari_eio.domain "foo.localhost" (respond_msg "foo")
        ~all:(respond_msg "*.foo");
      Mehari_eio.domain "bar.localhost" (respond_msg "bar");
    ]

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let cert =
    let ( / ) = Eio.Path.( / ) in
    X509_eio.private_of_pems ~cert:(env#cwd / "foo_cert.pem")
      ~priv_key:(env#cwd / "foo_key.pem")
  in
  Mehari_eio.run ~certs:(Single cert) router env
