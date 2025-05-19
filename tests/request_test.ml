open Mehari
open Mehari.Private

let ipaddr = Alcotest.testable Ipaddr.pp (fun i i' -> Ipaddr.compare i i' = 0)
let request_err = Alcotest.testable Protocol.pp_err Protocol.equal_err
let uri = Alcotest.testable Uri.pp Uri.equal

let mock_request client_request =
  Protocol.make_request
    ~client_ip:(Ipaddr.of_string_exn "80.120.170.10")
    ~client_port:8132
    ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
    ~port:1917 ~tls_version:`TLS_1_3 ~client_request ~now:(Ptime_clock.now ())
    ()

let client_ip_1 = Ipaddr.of_string_exn "80.120.170.10"

let request_1 =
  Protocol.make_request ~client_ip:client_ip_1 ~client_port:97416
    ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
    ~port:1917 ~tls_version:`TLS_1_3
    ~client_request:"gemini://localhost/foo/bar" ~now:(Ptime_clock.now ()) ()

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

let test_request_ip_1 =
  let open Alcotest in
  test_case "request URI test - 1" `Quick (fun () ->
      let expected = Result.map Request.ip request_1 in
      let computed = client_ip_1 in
      check (result ipaddr request_err) "should be equal" expected (Ok computed))

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

let test_request_invalid_client_cert =
  let open Alcotest in
  (* Generated with: openssl req -x509 -newkey rsa:4096 -sha256 -not_before 200801010000Z -not_after 201001010000Z -nodes *)
  let client_cert =
    "-----BEGIN CERTIFICATE-----\n\
     MIIFAzCCAuugAwIBAgIUJ5nCY43haKnDc5PB3NgmL6w8bbMwDQYJKoZIhvcNAQEL\n\
     BQAwETEPMA0GA1UEAwwGZm9vYmFyMB4XDTIwMDgwMTAxMDAwMFoXDTIwMTAwMTAx\n\
     MDAwMFowETEPMA0GA1UEAwwGZm9vYmFyMIICIjANBgkqhkiG9w0BAQEFAAOCAg8A\n\
     MIICCgKCAgEAtOpNmRZlsp19j9E3Ol2elUfL2ooK8axhEmgFOeklMKA5HnzkWyp3\n\
     KSl8PI3BRtRsXLr0JJtQUpKNAx1jAhk9XswNxh6wQW/GpwubvbHTwDYo5UP6u3pV\n\
     7q7aK0gZWqgFRR8JcoYJK5gCI8XwLKzhE4JXejmMhjEgYrkaym5oOlYu1HZX8BKM\n\
     z5b4xGgE0Hrd/Moz96BlGCLLoUYBt6BtK6lnYfSmN/s+VMo574Ns8fePy6V0E9/8\n\
     NmbIY62nk+15EUP5hhA8gbyUUofhnoV5sVm0OyvXqUzxZjfBX8/1szA7yfI5ihaL\n\
     ahnpOrdGQ/tCpkx3EWMsoGpT0uItfWzU07gqvuesdPMcBNXS9Kby7t8gD8TxSDn7\n\
     FjU/uS1A3hrGULGZ7nYMb0FVbKDekbofBl4dL47WEBTU0I6+wFSq2sv7S7WAwd++\n\
     d9nVywPG7JlD81fhcIh8gpyob2N/H0YqOBEmYZdDWjeQb5ATqr7BYXvtwkfKQxRY\n\
     56o7vFApWNGO8PW6NTJA8LDzB8CB7gycDqr98WZ/wXJ19hVqM7ftfqg4ddTgps5C\n\
     MEAFSHohYcL8UtljZ8JOIY8p2D5v5iwcDJiqkdWrUVGJO78JITPFALBk7OMJWP+p\n\
     qjLiI7J4SgjemAMcksNcfKHMx5jnOpsz+siZo8ivAoS3wWBNd9HlGpMCAwEAAaNT\n\
     MFEwHQYDVR0OBBYEFFJ0bt0nbT1Ucg8Y2Y7FMECvCJ7tMB8GA1UdIwQYMBaAFFJ0\n\
     bt0nbT1Ucg8Y2Y7FMECvCJ7tMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQEL\n\
     BQADggIBAGpavQF5Sx5IGsGdfmvPLJgBGtoxd87zPiTuC3RSdCOJNz6NwxWhEKmp\n\
     UlUbQgWDhAov9/zAnzJvriM2F1LISMd05Uw7GwAJ2rtkz0MYwexXs5bD54I+VcJd\n\
     h3m43z5DLIOvdoSv/KxFnvszzyUbTpgZFFdAndnbqjiB9UY3UMLBp50WufZQd8dh\n\
     uYOfL6ADrD3+4P3mElNUxvpUA2MSFLeoBwPSGuUd4IsvKxaBkVFfisfF6akvs0VS\n\
     x2qqg/MrUDhvNcFRyhax3hpzs7MyqWTA4Z8Smbx5zHQ/eBqillSkpZVTPFSc9v/p\n\
     TQwWLFsgzdfxjVqZlEqP4svPyCxXhybhP6LLo5GWh0JsB+iWl7pHqYmdIAbdyIfd\n\
     Rs/ncqk4vEU2Y8GAcuEYdrmj3WyZw+cbKEdDMe1X/S4Dv9TKPPaVrKmYxNLGkvRR\n\
     y4/iXzSlVBTIxL+XvZb0YUNV1E/haAYzqS1Y3q4P0OyVOPB2qfFtjD+Kio2Onqeb\n\
     Gat5uRkZttvLqCLu4vGLJldAvfC2JMlDFfZTnzhEpSOXHA8XxvZ5d8I1Or8Z8Xvm\n\
     1+ZVAta4GyTSZv2J4PJF695pcCr8jwaUa/jyvt9nszq0Qfi+ujejvdIm/PLk2qkK\n\
     DSKakziEsfoQ4dDEoPxkBi0tagep5wV/wh3RG+X09ouoc6G+g7GC\n\
     -----END CERTIFICATE-----" |> X509.Certificate.decode_pem |> Result.get_ok
  in
  test_case "request with invalid client certificate" `Quick (fun () ->
      let now = Ptime_clock.now () in
      let expected =
        Error
          (Protocol.ClientCertificateNotValid
             (`CACertificateExpired (client_cert, Some now)))
      in
      let computed =
        Protocol.make_request
          ~client_ip:(Ipaddr.of_string_exn "80.120.170.10")
          ~client_port:78751
          ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
          ~port:1917 ~tls_version:`TLS_1_3 ~client_cert
          ~client_request:"gemini://foobar.com/" ~now ()
      in
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

let test_request_ip_addr_hostname =
  let open Alcotest in
  test_case "request a valid IP address as hostname" `Quick (fun () ->
      let expected = Ok () in
      let computed =
        Protocol.make_request ~port:1917
          ~client_ip:(Ipaddr.of_string_exn "80.120.170.10")
          ~client_port:78954 ~tls_version:`TLS_1_3
          ~client_request:"gemini://80.120.170.11/test" ?hostname:None
          ~now:(Ptime_clock.now ()) ()
        |> Result.map (fun _ -> ())
      in
      check (result unit request_err) "should be equal" expected computed)

let test_request_invalid_ip_addr_hostname =
  let open Alcotest in
  test_case "request an invalid IP address as hostname" `Quick (fun () ->
      let expected = Error Protocol.NotADomainName in
      let computed =
        Protocol.make_request ~port:1917
          ~client_ip:(Ipaddr.of_string_exn "80.120.170.10")
          ~client_port:23654 ~tls_version:`TLS_1_3
          ~client_request:"gemini://80.120..170.11" ?hostname:None
          ~now:(Ptime_clock.now ()) ()
      in
      check (result pass request_err) "should be equal" expected computed)

let test_request_relative_uri_path =
  let open Alcotest in
  test_case "request URI with a relative path test" `Quick (fun () ->
      let expected = Error Protocol.RelativePath in
      let computed = mock_request "gemini://hello.foo" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_user_info =
  let open Alcotest in
  test_case "request with URL containing userinfo" `Quick (fun () ->
      let expected = Error Protocol.UserInfoNotAllowed in
      let computed = mock_request "gemini://tim:mdp@example.com/" in
      check (result reject request_err) "should be equal" expected computed)

let test_request_wrong_port =
  let open Alcotest in
  test_case "request with URL wrong port" `Quick (fun () ->
      let expected = Error Protocol.WrongPort in
      let computed =
        Protocol.make_request
          ~client_ip:(Ipaddr.of_string_exn "80.120.170.10")
          ~client_port:1254
          ~hostname:Domain_name.(of_string_exn "localhost" |> host_exn)
          ~tls_version:`TLS_1_3 ~port:1917
          ~client_request:"gemini://heyplzlookat.me:1848/"
          ~now:(Ptime_clock.now ()) ()
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
      test_request_ip_1;
      test_request_above_max_size;
      test_request_begin_with_bom;
      test_request_invalid_client_cert;
      test_request_empty_url;
      test_request_malformed_utf8;
      test_request_missing_host;
      test_request_missing_scheme;
      test_request_invalid_domain_name;
      test_request_ip_addr_hostname;
      test_request_invalid_ip_addr_hostname;
      test_request_relative_uri_path;
      test_request_user_info;
      test_request_wrong_port;
      test_request_wrong_scheme;
    ] )
