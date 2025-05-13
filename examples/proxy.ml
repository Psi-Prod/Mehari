(** To test this example, run:
    {@bash[
      echo -e "gemini://foo/" | openssl s_client -crlf -connect localhost:1965 -servername foo -ign_eof
    ]} *)

open Mehari

let router =
  Mehari_eio.virtual_hosts ~meth:`ByURL
    [
      ("foo", fun _ _ -> Response.text "foo");
      ("bar", fun _ _ -> Response.text "bar");
    ]

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let cert =
    let ( / ) = Eio.Path.( / ) in
    X509_eio.private_of_pems ~cert:(env#cwd / "cert.pem")
      ~priv_key:(env#cwd / "key.pem")
  in
  Mehari_eio.run ~certs:(Single cert) ~verify_url_host:false router env
