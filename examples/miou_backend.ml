open Mehari
open Mehari_miou_unix

let private_of_pems ~cert ~priv_key =
  let pem = In_channel.(with_open_text cert input_all) in
  match X509.Certificate.decode_pem_multiple pem with
  | Ok certs -> (
      let pem = In_channel.(with_open_text priv_key input_all) in
      match X509.Private_key.decode_pem pem with
      | Ok key -> (certs, key)
      | Error (`Msg msg) ->
          Printf.sprintf "Private key (%s): failed to parse private key %s"
            priv_key msg
          |> invalid_arg)
  | Error (`Msg msg) ->
      Printf.sprintf
        "Private certificates (%s): failed to parse certificates %s" cert msg
      |> invalid_arg

let any = Mehari.Path.variable ~from_string:Option.some ~to_string:Fun.id

let router =
  router
    [
      route Path.root (fun _ -> respond_document "./README.md");
      route Path.(~/"echo" /: any) (fun text _req -> Response.text text);
      route Path.(~/"sources" /: any) (static "./");
    ]

let () =
  set_log_lvl Info;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let cert = private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem" in
  router |> logger |> run ~certs:(Single cert)
