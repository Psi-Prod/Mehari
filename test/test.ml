let router =
  let open Mehari_eio in
  router
    [
      (* Request related functions and [Mehari.response_text] *)
      route "/request" (fun req ->
          String.concat "\n"
            [
              Mehari.uri req |> Uri.to_string;
              Mehari.target req;
              Mehari.port req |> Int.to_string;
              Mehari.sni req;
              (* Mehari.client_cert req; *)
              (* Mehari.param req 1; *)
            ]
          |> Mehari.response_text);
    ]

let header =
  let open Eio.Buf_read.Syntax in
  let crlf = Eio.Buf_read.string "\r\n" in
  Eio.Buf_read.take_while (fun c -> not (Char.equal c '\r')) <* crlf

let client net ~port ~host url =
  Eio.Net.with_tcp_connect net ~service:(string_of_int port) ~host (fun flow ->
      let host =
        Domain_name.of_string_exn host |> Domain_name.host |> Result.to_option
      in
      let tls_config =
        let null ?ip:_ ~host:_ _certs = Ok None in
        Tls.Config.client ~authenticator:null ()
      in
      let client = Tls_eio.client_of_flow tls_config ?host flow in
      Eio.Flow.copy_string (Uri.to_string url ^ "\r\n") client;
      Eio.Buf_read.parse_exn
        (Eio.Buf_read.pair header Eio.Buf_read.take_all)
        client ~max_size:Sys.max_string_length)

let pass () =
  let net = Eio_mock.Net.make "mock net" in
  let socket = Eio_mock.Flow.make "socket" in
  Eio_mock.Net.on_getaddrinfo net
    [ `Return [ `Tcp (Eio.Net.Ipaddr.V4.loopback, 1965) ] ];
  Eio_mock.Net.on_connect net [ `Return socket ];
  Eio_main.run (fun env ->
      Eio.Fiber.both
        (fun () ->
          Mehari_eio.run
            ~certchains:Eio.Path.[ (env#cwd / "cert.pem", env#cwd / "key.pem") ]
            (net :> Eio.Net.t)
            router)
        (fun () ->
          let buf = Buffer.create 8096 in
          let buf_sink = Eio.Flow.buffer_sink buf in
          Eio.Flow.copy socket buf_sink;
          Buffer.contents buf |> print_endline))

let raising () =
  assert (
    try[@warning "-52"]
      Mehari.(response input) "\u{FEFF}Foo" |> ignore;
      false
    with Invalid_argument "<META> begins with a U+FEFF byte order mark" ->
      true);
  assert (
    try[@warning "-52"]
      Mehari.(response input) (String.make 1024 'o') |> ignore;
      true
    with Invalid_argument "too long header" -> false);
  assert (
    try[@warning "-52"]
      Mehari.(response input) (String.make 1025 'o') |> ignore;
      false
    with Invalid_argument "too long header" -> true)

let () =
  Gemtext.pass ();
  raising ();
  pass ()
