let book =
  object
    val mutable entries = []

    method add_entry ~addr msg =
      entries <- (Unix.time () |> Unix.gmtime, addr, msg) :: entries

    method print =
      let buf = Buffer.create 4096 in
      List.iter
        (fun (timestamp, addr, msg) ->
          Format.kasprintf (Buffer.add_string buf)
            "%i-%i-%i %i:%i:%i - %a: %s\n"
            (timestamp.Unix.tm_year + 1900)
            (timestamp.tm_mon + 1) timestamp.tm_mday timestamp.tm_hour
            timestamp.tm_min timestamp.tm_sec Ipaddr.pp addr
            (Uri.pct_decode msg))
        entries;
      Buffer.contents buf
  end

module M_unix = Mehari_lwt_unix

let () =
  M_unix.router
    [
      M_unix.route "/" (fun _ ->
          Mehari.Gemtext.
            [
              heading `H1 "Guestbook";
              newline;
              link "/submit" ~name:"Submit a new entry";
              newline;
              heading `H2 "Entries:";
              text book#print;
            ]
          |> M_unix.respond_gemtext);
      M_unix.route "/submit" (fun req ->
          match Mehari.query req with
          | None -> M_unix.respond Mehari.input "Enter your message"
          | Some msg ->
              book#add_entry ~addr:(Mehari.ip req) msg;
              M_unix.respond Mehari.redirect_temp "/");
    ]
  |> M_unix.run
