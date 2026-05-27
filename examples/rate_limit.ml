open Mehari

let low_limit = Rate_limit.make 5 `Minute
let high_limit = Rate_limit.make ~period:10 2 `Second

let router =
  Router.router
    [
      Router.route
        Path.(~/"low")
        ~rate_limit:low_limit
        (fun _ -> Response.text "5 requests per minute authorized");
      Router.route
        Path.(~/"high")
        ~rate_limit:high_limit
        (fun _ -> Response.text "2 requests per 10 seconds authorized");
    ]

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
