let router =
  Mehari_eio.router
    [
      Mehari_eio.route "/" (fun req ->
          match Mehari.client_cert req with
          | [] -> Mehari.(response client_cert_req) "Certificate plz"
          | hd :: _ ->
              X509.Certificate.encode_pem hd
              |> Cstruct.to_string
              |> Printf.sprintf "Client certificate ~nyoron\n%s"
              |> Mehari.response_text);
    ]

let main ~net ~cwd =
  let certchains = Common.Eio.load_certchains cwd in
  Mehari_eio.run net ~certchains router

let () = Common.Eio.run_server main
