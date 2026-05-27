open Mehari

let count n =
  Seq.unfold
    (function
      | None -> None
      | Some i when Int.equal i n -> Some ("End", None)
      | Some i ->
          Miou_unix.sleep 1.;
          Some (Printf.sprintf "%i\n" i, Some (i + 1)))
    (Some 0)

let handler req =
  match Request.query req with
  | None -> Response.respond Status.input "Enter a number"
  | Some number ->
      begin match int_of_string_opt number with
      | None -> Response.respond Status.bad_request "Enter a valid number!"
      | Some n ->
          let body = Body.seq (count n) in
          Response.body body Mime.plaintext
      end

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger handler)
