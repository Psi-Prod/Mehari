type entry = { timestamp : Unix.tm; addr : Ipaddr.t; message : string }

let book =
  object
    val mutable entries = []

    method add_entry ~addr message =
      let timestamp = Unix.time () |> Unix.gmtime in
      entries <- { timestamp; addr; message } :: entries

    method print =
      let buf = Buffer.create 4096 in
      List.iter
        (fun e ->
          Format.kasprintf (Buffer.add_string buf)
            "%i-%i-%i %i:%i:%i - %a: %s\n"
            (e.timestamp.tm_year + 1900)
            (e.timestamp.tm_mon + 1) e.timestamp.tm_mday e.timestamp.tm_hour
            e.timestamp.tm_min e.timestamp.tm_sec Ipaddr.pp e.addr
            (Uri.pct_decode e.message))
        entries;
      Buffer.contents buf
  end

module M_unix = Mehari_lwt_unix

let () =
  M_unix.router
    [
      M_unix.route "/" (fun _ ->
          let home =
            Mehari.Gemtext.
              [
                heading `H1 "Guestbook";
                newline;
                link "/submit" ~name:"Submit a new entry";
                newline;
                text book#print;
              ]
          in
          M_unix.respond_gemtext home);
      M_unix.route "/submit" (fun req ->
          match Mehari.query req with
          | None -> M_unix.respond Mehari.input "Enter your message"
          | Some msg ->
              book#add_entry ~addr:(Mehari.ip req) msg;
              M_unix.respond Mehari.redirect_temp "/");
    ]
  |> M_unix.run
