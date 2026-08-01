(** To generate correct certificates to test this example, run:

    {@bash[
      mkcert localhost foo.localhost bar.localhost
    ]} *)

open Mehari

let respond_msg msg _req =
  Format.kasprintf Response.text "Requesting subdomain %s" msg

let router =
  Router.virtual_host
    [
      Router.domain "localhost" (fun _ ->
          Response.text "No subdomain requested");
      Router.domain "foo.localhost" (respond_msg "foo")
        ~all:(respond_msg "*.foo");
      Router.domain "bar.localhost" (respond_msg "bar");
    ]

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
