let () =
  Alcotest.run "mehari.tests"
    [ Cgi_test.cases; Mime_test.cases; Request_test.cases ]
