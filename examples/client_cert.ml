open Mehari

let router =
  Router.router
    [
      Router.route Path.root (fun req ->
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
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
