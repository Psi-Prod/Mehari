open Mehari
open Mehari_miou

let router =
  Router.router
    [
      Router.route Path.root (fun _ -> respond_document "./README.md");
      Router.route
        Path.(~/"cgi")
        (run_cgi ~non_parsed:true "./examples/cgi_script.py");
      Router.route
        Path.(~/"echo" /: string)
        (fun text _req -> Response.text text);
      Router.route Path.(~/"sources" /: string) (static "./");
    ]

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)

let r =
  Mehari.(
    Router.route
      Path.(~/"articles" /: string)
      (fun article req ->
        Response.text (Printf.sprintf "Get article %S" article)))
