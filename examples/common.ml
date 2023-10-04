open Lwt.Infix

module Lwt = struct
  let load_certchains () =
    X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
    >|= fun cert -> [ cert ]
end

module Eio = struct
  let load_certchains cwd =
    Eio.Path.
      [
        X509_eio.private_of_pems ~cert:(cwd / "cert.pem")
          ~priv_key:(cwd / "key.pem");
      ]

  let run_server serve =
    Eio_main.run @@ fun env ->
    Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env
    @@ fun () -> serve ~net:env#net ~cwd:env#cwd
end
