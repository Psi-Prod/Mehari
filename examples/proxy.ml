(** To test this example, run:
    {@bash[
      echo -e "gemini://foo/" | openssl s_client -crlf -connect localhost:1965 -servername foo -ign_eof
    ]} *)

open Mehari

let router =
  Mehari_eio.virtual_hosts ~meth:`ByURL
    [
      ("foo", fun _ -> Response.text "foo");
      ("bar", fun _ -> Response.text "bar");
    ]

let main ~net ~cwd =
  let certchains = Common.Eio.load_certchains cwd in
  Mehari_eio.run net ~certchains ~verify_url_host:false router

let () = Common.Eio.run_server main
