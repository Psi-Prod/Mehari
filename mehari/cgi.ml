type t = {
  auth_type : string;
  content_length : string;
  content_type : string;
  gateway_interface : string;
  path_info : string;
  path_translated : string;
  query_string : string;
  remote_addr : string;
  remote_host : string;
  remote_ident : string;
  remote_method : string;
  remote_user : string;
  request_method : string;
  script_name : string;
  server_name : string;
  server_port : string;
  server_protocol : string;
  server_software : string;
  tls_client_hash : string;
  tls_client_subject : string;
  tls_client_issuer : string;
}

let or_empty = Option.value ~default:""

let make req ~script_path ~server_addr =
  let client_cert = Request.client_cert req in
  let auth_type =
    client_cert |> Option.map (fun _ -> "Certificate") |> or_empty
  in
  let path_info = Request.target req |> Uri.pct_decode in
  let query_string = Request.query req |> or_empty in
  let client_addr = Request.ip req |> Ipaddr.to_string in
  let server_name =
    match Request.uri req |> Uri.host with
    | Some hostname -> hostname
    | None -> Ipaddr.to_string server_addr
  in
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
  {
    auth_type;
    content_length = "";
    content_type = "";
    gateway_interface = "CGI/1.1";
    path_info;
    path_translated = path_info;
    query_string;
    remote_addr = client_addr;
    remote_host = client_addr;
    remote_ident = "";
    remote_method = "";
    remote_user = "";
    request_method = "";
    script_name = script_path;
    server_name;
    server_port;
    server_protocol = "GEMINI";
    server_software = "Mehari/%%VERSION%%";
    tls_client_hash;
    tls_client_subject;
    tls_client_issuer;
  }

let to_env t =
  [|
    ("AUTH_TYPE", t.auth_type);
    ("CONTENT_LENGTH", t.content_length);
    ("CONTENT_TYPE", t.content_type);
    ("GATEWAY_INTERFACE", t.gateway_interface);
    ("PATH_INFO", t.path_info);
    ("PATH_TRANSLATED", t.path_translated);
    ("QUERY_STRING", t.query_string);
    ("REMOTE_ADDR", t.remote_addr);
    ("REMOTE_HOST", t.remote_host);
    ("REMOTE_IDENT", t.remote_ident);
    ("REMOTE_USER", t.remote_user);
    ("REQUEST_METHOD", t.request_method);
    ("SCRIPT_NAME", t.script_name);
    ("SERVER_NAME", t.server_name);
    ("SERVER_PORT", t.server_port);
    ("SERVER_PROTOCOL", t.server_protocol);
    ("SERVER_SOFTWARE", t.server_software);
    ("TLS_CLIENT_HASH", t.tls_client_hash);
    ("TLS_CLIENT_SUBJECT", t.tls_client_subject);
    ("TLS_CLIENT_ISSUER", t.tls_client_issuer);
  |]
