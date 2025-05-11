open Mehari

let router =
  Mehari_eio.router
    [
      Mehari_eio.route "/" (fun req ->
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

let main ~net ~cwd =
  let certchains = Common.Eio.load_certchains cwd in
  Mehari_eio.run net ~certchains router

let () = Common.Eio.run_server main
