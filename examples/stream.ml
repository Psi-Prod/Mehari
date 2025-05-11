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

let router clock req =
  match Request.query req with
  | None -> Response.respond Status.input "Enter a number"
  | Some number -> (
      match int_of_string_opt number with
      | None -> Response.respond Status.bad_request "Enter a valid number!"
      | Some n ->
          let body = count clock n |> Response.Body.seq ~flush:true in
          Response.body body Mime.plaintext)

let main ~clock ~cwd ~net =
  let certchains = Common.Eio.load_certchains cwd in
  Mehari_eio.run net ~certchains (router clock)

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  main ~clock:env#clock ~cwd:env#cwd ~net:env#net
