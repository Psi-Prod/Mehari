open Mehari

let n = Atomic.make 0

let router =
  Router.router
    [
      Router.route Path.root (fun _ ->
          Atomic.incr n;
          Logs.info ~src:Logger.src (fun log ->
              log "Request n°: %i" (Atomic.get n));
          Response.text "This request is logged");
    ]

let () =
  Logs.Src.set_level Logger.src (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
