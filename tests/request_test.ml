open Mehari
open Mehari.Private

let uri = Alcotest.testable Uri.pp Uri.equal
let request_err = Alcotest.testable Protocol.pp_err Protocol.equal_err

let mock_request ?(certs = []) client_request =
  Protocol.make_request
    ~client_addr:(Ipaddr.of_string_exn "80.120.170.10")
    ~server_addr:Ipaddr.(V4 V4.localhost)
    ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
    ~port:1917 ~verify_url_host:true ~tls_version:`TLS_1_3 ~client_request certs

let request_1 =
  Protocol.make_request
    ~client_addr:(Ipaddr.of_string_exn "80.120.170.10")
    ~server_addr:Ipaddr.(V4 V4.localhost)
    ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
    ~port:1917 ~verify_url_host:false ~tls_version:`TLS_1_3
    ~client_request:"gemini://localhost/foo/bar" []

(* Generated with :
   openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -sha256
   -days 365 -nodes --subj "/CN=heyplzlookat.me" *)
let pem_example =
  "-----BEGIN CERTIFICATE-----\n\
   MIIFFTCCAv2gAwIBAgIUPyXv74fRRdTmmN7ZpnfzrYqoXAYwDQYJKoZIhvcNAQEL\n\
   BQAwGjEYMBYGA1UEAwwPaGV5cGx6bG9va2F0Lm1lMB4XDTI1MDUxMDExMjcyMVoX\n\
   DTI2MDUxMDExMjcyMVowGjEYMBYGA1UEAwwPaGV5cGx6bG9va2F0Lm1lMIICIjAN\n\
   BgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAuo3DMv/7edtq8ZXZVh8JcZmdtRKS\n\
   swiuVxx2eLvBKPJE2rCbBcQHP0Nq2KREfH+Woswu93Daa0LYL7EKW6UQM/5xfqwf\n\
   BczzOU5TKJ0EdvCLcFanMyUAFYOC69Qj522JZAEvX/WtikHZUjS5bLrDgzeed4+j\n\
   KDeasNws5o48ttrMdUc6qNC0DjVigmaCOEopPVJ3eUxNKpffddfujCdbjjJnk6eD\n\
   SvMoWryQeKOoXYPrJI+ptBEttcxOmX4rkBzN6xvRP7Xw+IQpm5NkJwR4GLsSgpN+\n\
   B8tEStgSUusFj7zvkEDSgyPBu6ylCIIqWlay6BpDZkg4DU4WcHMTAf1o2BbpZQC8\n\
   t9uVSNaz7Fs1KSRmuDtFmGUJL2f/fOhB5U8Mul/37BUTGsDlvR76GTM1QaqUmqw1\n\
   kzAQ4skmSaueStVXJWe+61jRXkEQTq/D58L/YTQkhUnG98ghaKPBrADxjN5/Muv+\n\
   ARR/5nFevO1rttU2cR7T+BNsV+8/WZOIpfjsXESo3QrD+Sa4qPwxEHxJ1jqPJoBt\n\
   qiXZJMwnqRVkADarB56lBxU7fNZdaYph/mhqN/qHKVzvHA1ELeL0aefUVN+CDSKH\n\
   jLnykY+h5g+ypKh8aYOY68YvJtKoV5RiJ1lcOJ6gV6c6GcQDcfe/GzA0zUVXmjlu\n\
   TSxYhyApES18KIUCAwEAAaNTMFEwHQYDVR0OBBYEFKE0W7XfqKytFmQfOee3W+gu\n\
   1E/bMB8GA1UdIwQYMBaAFKE0W7XfqKytFmQfOee3W+gu1E/bMA8GA1UdEwEB/wQF\n\
   MAMBAf8wDQYJKoZIhvcNAQELBQADggIBAH1pDYJhOlya5zIbj2MtZgS+WyviHk77\n\
   8b8BgeS99PYxW+XEESOht35A8ifXSsv2bHeKCd4x+H4iOizfmty4IMVXSxyq/0j2\n\
   XqEgUUabBhyTPircLfxvo+TMBTwq1ZpAezJpWpWCpdLl8KlthSuumbW0f2hUfjRk\n\
   rU+TZPXIgGfLogBNFiYclJbgCU4r2FAhLOj9ZB9/9Y3IPYr64OSeY3cYeWMs0P1l\n\
   m2OfhpmPcqec9KCSn+GnFFfzK3pD1VHDI6w00fNfpBke4AbQ0aBp9h0V6X3MC8a3\n\
   QJW4TqfS4IIedgTqbtLjGPv4civ3WwFB3p6t6LVWrq8UnM9onBM4mPuYPlzC79I7\n\
   Vb/D1W+Yql0PaerxqBS7leUSXTOkTe8GkLgcLaQQQJy+EwVOiBBYLlAn9zVDxSBL\n\
   G3cVjOI7CQmnqgnSSSr4tHsVNwh5nCFsoQVQh8bHM3dxESwRWCgIq2Ctm8ZQ2xH3\n\
   PpZAwuxY5390hQPWAbwY+6FyRJP6EK8608kviS/e6ktL4rnLpaINHUtsbeB5blRv\n\
   kX0p75iYzE4BqpO1qz9ae5s3Ktyqq4lNlNiPk+0u4ak0QQkziXmFKSTUluNoaR3/\n\
   w1mpGQhurSSSwgceggEpW+HvfBtjo0dwtqeNNExt5j4UtVkAeHwvIZFijohQDrDm\n\
   77uQcZGLxeCt\n\
   -----END CERTIFICATE-----"

let test_request_uri_1 =
  let open Alcotest in
  test_case "request URI test - 1" `Quick (fun () ->
      let expected = Result.map Request.uri request_1 in
      let computed = Uri.of_string "gemini://localhost/foo/bar" in
      check (result uri request_err) "should be equal" expected (Ok computed))

let test_request_target_1 =
  let open Alcotest in
  test_case "request target test - 1" `Quick (fun () ->
      let expected = Result.map Request.target request_1 in
      let computed = Ok "/foo/bar" in
      check (result string request_err) "should be equal" expected computed)

let test_request_query_1 =
  let open Alcotest in
  test_case "request query test - 1" `Quick (fun () ->
      let expected = Result.map Request.query request_1 in
      let computed = Ok None in
      check
        (result (option string) request_err)
        "should be equal" expected computed)

let test_request_above_max_size =
  let open Alcotest in
  test_case "request above max size test" `Quick (fun () ->
      let expected = Error Protocol.AboveMaxSize in
      let computed =
        let address = "gemini://foo.lol/" in
        mock_request
          (address ^ String.make (1024 - String.length address + 1) 'w')
      in
      check (result reject request_err) "should be equal" expected computed)

let test_request_begin_with_bom =
  let open Alcotest in
  test_case "request begin with BOM test" `Quick (fun () ->
      let expected = Error Protocol.BeginWithBOM in
      let computed = mock_request "\u{feff}" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_empty_url =
  let open Alcotest in
  test_case "request with empty URL test" `Quick (fun () ->
      let expected = Error Protocol.EmptyURL in
      let computed = mock_request "" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_malformed_utf8 =
  let open Alcotest in
  test_case "request containing malformed UTF-8 test" `Quick (fun () ->
      let expected = Error Protocol.MalformedUTF8 in
      let computed = mock_request "\xED\xBF\xBF" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_missing_host =
  let open Alcotest in
  test_case "request URI with missing host test" `Quick (fun () ->
      let expected = Error Protocol.MissingHost in
      let computed = mock_request "gemini:///foo.gmi" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_missing_scheme =
  let open Alcotest in
  test_case "request URI with missing scheme test" `Quick (fun () ->
      let expected = Error Protocol.MissingScheme in
      let computed = mock_request "heyplzlookat.me/gemlog.gmi" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_invalid_domain_name =
  let open Alcotest in
  test_case "request URI with invalid domain name test" `Quick (fun () ->
      let expected = Error Protocol.NotADomainName in
      let computed = mock_request "gemini://foo..mdr/" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_relative_uri_path =
  let open Alcotest in
  test_case "request URI with a relative path test" `Quick (fun () ->
      let expected = Error Protocol.RelativePath in
      let computed = mock_request "gemini://hello.foo" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_sni_required =
  let open Alcotest in
  test_case "request without SNI extension test" `Quick (fun () ->
      let expected = Error Protocol.SNIExtRequired in
      let computed =
        Protocol.make_request ~port:1917
          ~client_addr:(Ipaddr.of_string_exn "80.120.170.10")
          ~server_addr:Ipaddr.(V4 V4.localhost)
          ~verify_url_host:true ~tls_version:`TLS_1_3 ~client_request:"" []
          ?hostname:None
      in
      check (result reject request_err) "should be equal" expected computed)

let test_request_user_info =
  let open Alcotest in
  test_case "request with URL containing userinfo" `Quick (fun () ->
      let expected = Error Protocol.UserInfoNotAllowed in
      let computed = mock_request "gemini://tim:mdp@example.com/" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_wrong_host =
  let open Alcotest in
  test_case
    "request with URL hostname which doesn't match certificates hostnames"
    `Quick (fun () ->
      let expected = Error Protocol.WrongHost in
      let computed =
        let certs =
          X509.Certificate.decode_pem_multiple pem_example |> Result.get_ok
        in
        mock_request ~certs "gemini://heyplzlookat.lol/"
      in
      check (result reject request_err) "should be equal" expected computed)

let test_request_wrong_port =
  let open Alcotest in
  test_case "request with URL wrong port" `Quick (fun () ->
      let expected = Error Protocol.WrongPort in
      let computed =
        Protocol.make_request
          ~client_addr:(Ipaddr.of_string_exn "80.120.170.10")
          ~server_addr:Ipaddr.(V4 V4.localhost)
          ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
          ~verify_url_host:false ~tls_version:`TLS_1_3 [] ~port:1917
          ~client_request:"gemini://heyplzlookat.me:1848/"
      in
      check (result reject request_err) "should be equal" expected computed)

let test_request_wrong_scheme =
  let open Alcotest in
  test_case "request with URL wrong scheme" `Quick (fun () ->
      let expected = Error Protocol.WrongScheme in
      let computed = mock_request "https://heyplzlookat.me/" in
      check (result reject request_err) "should be equal" expected computed)

let cases =
  ( "request_test",
    [
      test_request_uri_1;
      test_request_target_1;
      test_request_query_1;
      test_request_above_max_size;
      test_request_begin_with_bom;
      test_request_empty_url;
      test_request_malformed_utf8;
      test_request_missing_host;
      test_request_missing_scheme;
      test_request_invalid_domain_name;
      test_request_relative_uri_path;
      test_request_sni_required;
      test_request_user_info;
      test_request_wrong_host;
      test_request_wrong_port;
      test_request_wrong_scheme;
    ] )
