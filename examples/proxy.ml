(** To test this example, run:
{@bash[
echo -e "gemini://foo/" | openssl s_client -crlf -connect localhost:1965 -servername foo -ign_eof
]} *)

let router =
  Mehari_eio.virtual_hosts ~meth:`ByURL
    [
      ("foo", fun _ -> Mehari.response_text "foo");
      ("bar", fun _ -> Mehari.response_text "bar");
    ]

let main ~net ~cwd =
  let certchains = Common.Eio.load_certchains cwd in
  Mehari_eio.run net ~certchains ~verify_url_host:false router

let () = Common.Eio.run_server main
