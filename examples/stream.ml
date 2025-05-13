open Mehari

let count clock n =
  Seq.unfold
    (function
      | None -> None
      | Some i when Int.equal i n -> Some ("End", None)
      | Some i ->
          Eio.Time.sleep clock 1.;
          Some (Printf.sprintf "%i\n" i, Some (i + 1)))
    (Some 0)

let router req env =
  match Request.query req with
  | None -> Response.respond Status.input "Enter a number"
  | Some number -> (
      match int_of_string_opt number with
      | None -> Response.respond Status.bad_request "Enter a valid number!"
      | Some n ->
          let body = count env#clock n |> Response.Body.seq ~flush:true in
          Response.body body Mime.plaintext)

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let cert =
    let open Eio.Path in
    X509_eio.private_of_pems ~cert:(env#cwd / "cert.pem")
      ~priv_key:(env#cwd / "key.pem")
  in
  Mehari_eio.run ~certchains:[ cert ] router env
