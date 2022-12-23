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
  match Mehari.query req with
  | None -> Mehari.(response input) "Enter a number"
  | Some number -> (
      match int_of_string_opt number with
      | None -> Mehari.(response bad_request) "Enter a valid number!"
      | Some n ->
          let body = count clock n |> Mehari.seq ~flush:true in
          Mehari.(response_body body plaintext))

let main ~clock ~cwd ~net =
  Mehari_eio.run net
    ~certchains:Eio.Path.[ (cwd / "cert.pem", cwd / "key.pem") ]
    (router clock)

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  main ~clock:env#clock ~cwd:env#cwd ~net:env#net
