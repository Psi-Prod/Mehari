open Mehari.Private

let test_cgi_env_1 =
  let open Alcotest in
  test_case "CGI env test - 1" `Quick (fun () ->
      let server_addr = Ipaddr.(V4 (V4.of_string_exn "10.10.200.130")) in
      let req =
        Protocol.make_request
          ~client_addr:(Ipaddr.of_string_exn "80.0.10.30")
          ~server_addr
          ~hostname:Domain_name.(of_string_exn "heyplzlookat.me" |> host_exn)
          ~port:1968 ~verify_url_host:false ~tls_version:`TLS_1_2
          ~client_request:"gemini://heyplzlookat.me/articles/mehari-0-3.gmi" []
        |> Result.get_ok
      in
      let expected =
        [|
          ("AUTH_TYPE", "");
          ("CONTENT_LENGTH", "");
          ("CONTENT_TYPE", "");
          ("GATEWAY_INTERFACE", "CGI/1.1");
          ("PATH_INFO", "/articles/mehari-0-3.gmi");
          ("PATH_TRANSLATED", "/articles/mehari-0-3.gmi");
          ("QUERY_STRING", "");
          ("REMOTE_ADDR", "80.0.10.30");
          ("REMOTE_HOST", "80.0.10.30");
          ("REMOTE_IDENT", "");
          ("REMOTE_USER", "");
          ("REQUEST_METHOD", "");
          ("SCRIPT_NAME", "./cgi-bin/cgi_script.py");
          ("SERVER_NAME", "heyplzlookat.me");
          ("SERVER_PORT", "1968");
          ("SERVER_PROTOCOL", "GEMINI");
          ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
          ("TLS_CLIENT_HASH", "");
          ("TLS_CLIENT_SUBJECT", "");
          ("TLS_CLIENT_ISSUER", "");
        |]
      in
      let computed =
        Cgi.make req ~script_path:"./cgi-bin/cgi_script.py" ~server_addr
        |> Cgi.to_env
      in
      check (array (pair string string)) "should be equal" expected computed)

let test_cgi_env_2 =
  let open Alcotest in
  test_case "CGI env test - 2" `Quick (fun () ->
      let server_addr = Ipaddr.(V4 V4.localhost) in
      let req =
        Protocol.make_request
          ~client_addr:(Ipaddr.of_string_exn "120.8.50.12")
          ~server_addr
          ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
          ~port:1965 ~verify_url_host:false ~tls_version:`TLS_1_3
          ~client_request:"gemini://localhost/a-very-bad-man.gmi?some_input" []
        |> Result.get_ok
      in
      let expected =
        [|
          ("AUTH_TYPE", "");
          ("CONTENT_LENGTH", "");
          ("CONTENT_TYPE", "");
          ("GATEWAY_INTERFACE", "CGI/1.1");
          ("PATH_INFO", "/a-very-bad-man.gmi");
          ("PATH_TRANSLATED", "/a-very-bad-man.gmi");
          ("QUERY_STRING", "some_input");
          ("REMOTE_ADDR", "120.8.50.12");
          ("REMOTE_HOST", "120.8.50.12");
          ("REMOTE_IDENT", "");
          ("REMOTE_USER", "");
          ("REQUEST_METHOD", "");
          ("SCRIPT_NAME", "/usr/lib/cgi-bin/cgi_script.pl");
          ("SERVER_NAME", "localhost");
          ("SERVER_PORT", "1965");
          ("SERVER_PROTOCOL", "GEMINI");
          ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
          ("TLS_CLIENT_HASH", "");
          ("TLS_CLIENT_SUBJECT", "");
          ("TLS_CLIENT_ISSUER", "");
        |]
      in
      let computed =
        Cgi.make req ~script_path:"/usr/lib/cgi-bin/cgi_script.pl" ~server_addr
        |> Cgi.to_env
      in
      check (array (pair string string)) "should be equal" expected computed)

let test_cgi_env_3 =
  let open Alcotest in
  test_case "CGI env test - 3" `Quick (fun () ->
      let client_cert =
        "-----BEGIN CERTIFICATE-----\n\
         MIICpTCCAY0CCCDJEJOo5ojXMA0GCSqGSIb3DQEBCwUAMBQxEjAQBgNVBAMMCWxp\n\
         bCBvY2FtbDAgFw0yNTA1MTAxMjI2MTJaGA85OTk5MTIzMTIzNTk1OVowFDESMBAG\n\
         A1UEAwwJbGlsIG9jYW1sMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA\n\
         7T4vxBRYZpu2sjH+s6qEtY4WYIwMxVYq1M4tK+mNmVby4uayZdTZF7OM97BY+z+r\n\
         +eVkIDv1nop631+njqAeWJzMny1AQzCvDqV/C1TJaL040S8hdveKmqhQbHqSEg99\n\
         e4XJloiBjlsDChKOO6czvcyAtaz1wFE31FLcUmH203tU8sa37KAeUphXlb/HJm1c\n\
         9s9IVaHZQXE8YQhTryzm/6uzWD5i2gxocD7PnO2ENw8iXtvvUhYHnL2BHWJKrlBi\n\
         /Pt1qYCCvVFwX8d9dg3Edp4aB/you/zaFRM9w371Yu9E/YNSu1wD+pbI200ndngc\n\
         K49h2Ysd27lpPGzr1sEQ8wIDAQABMA0GCSqGSIb3DQEBCwUAA4IBAQB1V7YUKz/H\n\
         EVVFZknccMJpHrQIbUURDsaxMCpNyGguecu17L5zCE1hSPFBQy0J/gCPOs/sfJjJ\n\
         keIJpSJT+pFjesPuihZJ7SaIxewg0nUbLULiF0yNykA/Jz0zFws7bJ02sA5NH++M\n\
         uxhVUJVt3KOZ6dNPCFQUEMvqZJFmtVNABvJIEfegshK9z7OPhOfagu4ijeWj5u5e\n\
         YJhKUG1yQ8A8BR1XgLj0+txtQKKhfEgFAtfngv5SFvu7b/J87NCvx3dRnEwgCrIG\n\
         d51Ix4kuWMi8ftCK1B+hZAy0Ej2lWfFuSkPwewFpoVk22mBIyz2EfVDzm3BqZ92q\n\
         rDgl8ph2PSa9\n\
         -----END CERTIFICATE-----" |> X509.Certificate.decode_pem
        |> Result.get_ok
      in
      let server_addr = Ipaddr.(V4 (V4.of_string_exn "152.19.95.83")) in
      let req =
        Protocol.make_request
          ~client_addr:(Ipaddr.of_string_exn "80.0.10.160")
          ~server_addr
          ~hostname:Domain_name.(of_string_exn "geminiprotocol.net" |> host_exn)
          ~port:1965 ~verify_url_host:false ~tls_version:`TLS_1_3 ~client_cert
          ~client_request:
            "gemini://geminiprotocol.net/docs/protocol-specification.gmi"
          []
        |> Result.get_ok
      in
      let expected =
        [|
          ("AUTH_TYPE", "Certificate");
          ("CONTENT_LENGTH", "");
          ("CONTENT_TYPE", "");
          ("GATEWAY_INTERFACE", "CGI/1.1");
          ("PATH_INFO", "/docs/protocol-specification.gmi");
          ("PATH_TRANSLATED", "/docs/protocol-specification.gmi");
          ("QUERY_STRING", "");
          ("REMOTE_ADDR", "80.0.10.160");
          ("REMOTE_HOST", "80.0.10.160");
          ("REMOTE_IDENT", "");
          ("REMOTE_USER", "");
          ("REQUEST_METHOD", "");
          ("SCRIPT_NAME", "/foo/bar/foobar.ml");
          ("SERVER_NAME", "geminiprotocol.net");
          ("SERVER_PORT", "1965");
          ("SERVER_PROTOCOL", "GEMINI");
          ("SERVER_SOFTWARE", "Mehari/%%VERSION%%");
          ( "TLS_CLIENT_HASH",
            "G\016\147\221Zk\141\024\204\132\201\022Pp\131\170\024-\016\189*\163F\031\238\219E\160E\184\147\196"
          );
          ("TLS_CLIENT_SUBJECT", "lil ocaml");
          ("TLS_CLIENT_ISSUER", "lil ocaml");
        |]
      in
      let computed =
        Cgi.make req ~script_path:"/foo/bar/foobar.ml" ~server_addr
        |> Cgi.to_env
      in
      check (array (pair string string)) "should be equal" expected computed)

let cases =
  ("cgi_test", [ test_cgi_env_1; test_cgi_env_2; test_cgi_env_3; cgi_test_1 ])
