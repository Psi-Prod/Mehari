open Mehari

let book =
  object
    val mutable entries = []

    method add_entry ~addr msg =
      entries <- (Mirage_ptime.now (), addr, msg) :: entries

    method print =
      let buf = Buffer.create 4096 in
      List.iter
        (fun (ptime, addr, msg) ->
          let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime in
          Format.kasprintf (Buffer.add_string buf)
            "%i-%i-%i %i:%i:%i - %a: %s\n" y m d hh mm ss Ipaddr.pp addr
            (Uri.pct_decode msg))
        entries;
      Buffer.contents buf
  end

let router =
  let open Router in
  router
    [
      route Path.root (fun _ ->
          Gemtext.
            [
              heading `H1 "Guestbook";
              newline;
              link "/submit" ~name:"Submit a new entry";
              newline;
              heading `H2 "Entries:";
              text book#print;
            ]
          |> Response.gemtext);
      route
        Path.(~/"submit")
        (fun req ->
          match Request.query req with
          | None -> Response.respond Status.input "Enter your message"
          | Some msg ->
              book#add_entry ~addr:(Request.ip req) msg;
              Response.respond Status.redirect_temp "/");
    ]

let () =
  Miou_unix.run @@ fun () ->
  Mirage_crypto_rng_unix.use_default ();
  let certs = Common.load_certs ~cert:"cert.pem" ~priv_key:"key.pem" in
  Mehari_miou.run ~certs
    Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))
    (Logger.logger router)
