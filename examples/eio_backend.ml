open Mehari

let any = Path.variable ~from_string:Option.some ~to_string:Fun.id

let router =
  Mehari_eio.router
    [
      Mehari_eio.route
        Path.(root)
        (fun _ env ->
          Mehari_eio.respond_document Eio.Path.(env#cwd / "README.md") env);
      Mehari_eio.route
        Path.(~/"sources" /: any)
        (fun target req env -> Mehari_eio.static env#cwd target req env);
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
