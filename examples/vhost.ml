let router =
  Mehari_eio.virtual_hosts
    [
      ("localhost.foo", fun _ -> Mehari.response_text "Requesting subdomain foo");
      ("localhost.bar", fun _ -> Mehari.response_text "Requesting subdomain bar");
    ]

let main ~net ~cwd =
  let certchains =
    let ( / ) = Eio.Path.( / ) in
    [
      X509_eio.private_of_pems ~cert:(cwd / "cert_foo.pem")
        ~priv_key:(cwd / "key_foo.pem");
      X509_eio.private_of_pems ~cert:(cwd / "cert_bar.pem")
        ~priv_key:(cwd / "key_bar.pem");
    ]
  in
  Mehari_eio.run net ~certchains router

let () = Common.Eio.run_server main
