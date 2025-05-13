let router =
  Mehari_eio.router
    [
      Mehari_eio.route "/" (fun _ env ->
          Mehari_eio.respond_document Eio.Path.(env#cwd / "README.md") env);
      Mehari_eio.route ~regex:true "/sources/(.*)" (fun req env ->
          Mehari_eio.static env#cwd req env);
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
