open Mehari

let counter = ref 0

let incr_count handler req =
  incr counter;
  handler req

let router =
  Router.router
    [
      Router.route Path.root (fun _ ->
          Response.gemtext
            [
              Gemtext.link "/incr" ~name:"Increment counter";
              Gemtext.text (Printf.sprintf "Counter = %i" !counter);
            ]);
      Router.route
        Path.(~/"incr")
        ~middlewares:[ incr_count ]
        (fun _ -> Response.respond Status.redirect_temp "/");
    ]

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
