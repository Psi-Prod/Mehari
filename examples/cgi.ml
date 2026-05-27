open Mehari

let router =
  let open Router in
  router
    [ route Path.(~/"cgi") (Mehari_miou.run_cgi "./examples/cgi_script.py") ]

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
