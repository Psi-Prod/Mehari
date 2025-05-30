open Mehari

let router =
  Mehari_eio.router
    [
      Mehari_eio.route Path.root (fun req _ ->
          match Request.client_cert req with
          | None -> Response.respond Status.client_cert_req "Certificate plz"
          | Some cert ->
              let pem = X509.Certificate.encode_pem cert in
              let common_name =
                X509.Certificate.subject cert
                |> X509.Distinguished_name.common_name
                |> Option.value ~default:"None"
              in
              Printf.sprintf "Ur client certificate ~nyoron\n%sCommon name: %s"
                pem common_name
              |> Response.text);
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
