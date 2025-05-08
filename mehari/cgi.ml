let make_env req ~fullpath ~path =
  let or_empty = Option.value ~default:"" in
  let client_cert =
    (* TODO: ensure that client_cert is unique *)
    match Request.client_cert req with
    | [] -> None
    | c :: _ -> Some c (* We pick the first one. *)
  in
  let auth_type, remote_user =
    match client_cert with
    | None -> ("", "")
    | Some c ->
        let cert_common_name =
          X509.Certificate.hostnames c
          |> X509.Host.Set.choose |> snd
          (* TODO: handle wildcard or strict *)
          |> Domain_name.to_string
        in
        (cert_common_name, "Certificate")
  in
  let path_info = Request.target req |> Uri.pct_decode in
  let query_string = Request.query req |> or_empty in
  let client_addr = Format.asprintf "%a" Ipaddr.pp @@ Request.ip req in
  let server_name = Request.uri req |> Uri.host |> or_empty in
  let server_port = Request.port req |> Int.to_string in
  let tls_client_hash, tls_client_subject, tls_client_issuer =
    match client_cert with
    | None -> ("", "", "")
    | Some c ->
        let hash = X509.Certificate.fingerprint `SHA256 c in
        let subject =
          X509.Certificate.subject c |> X509.Distinguished_name.common_name
          |> or_empty
        in
        let issuer =
          X509.Certificate.issuer c |> X509.Distinguished_name.common_name
          |> or_empty
        in
        (hash, subject, issuer)
  in
  [|
    ("AUTH_TYPE", auth_type);
    ("CONTENT_LENGTH", "");
    ("CONTENT_TYPE", "");
    ("GATEWAY_INTERFACE", "CGI/1.1");
    ("PATH_INFO", path_info);
    ("PATH_TRANSLATED", path);
    ("QUERY_STRING", query_string);
    ("REMOTE_ADDR", client_addr);
    ("REMOTE_HOST", client_addr);
    ("REMOTE_IDENT", "");
    ("REQUEST_METHOD", "");
    ("REMOTE_USER", remote_user);
    ("SCRIPT_NAME", fullpath);
    ("SERVER_NAME", server_name);
    ("SERVER_PORT", server_port);
    ("SERVER_PROTOCOL", "GEMINI");
    ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
    ("TLS_CLIENT_HASH", tls_client_hash);
    ("TLS_CLIENT_SUBJECT", tls_client_subject);
    ("TLS_CLIENT_ISSUER", tls_client_issuer);
  |]
  |> Array.map (fun (name, value) -> Printf.sprintf "%s=%s" name value)
