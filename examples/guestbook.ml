let book =
  object
    val mutable entries = []

    method add_entry ~addr msg =
      entries <- (Ptime_clock.now (), addr, msg) :: entries

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

open Mehari
open Lwt.Syntax
module M = Mehari_lwt_unix

let () =
  Lwt_main.run
    begin
      let* cert =
        X509_lwt.private_of_pems ~cert:"cert.pem" ~priv_key:"key.pem"
      in
      M.router
        [
          M.route Path.root (fun _ ->
              Gemtext.
                [
                  heading `H1 "Guestbook";
                  newline;
                  link "/submit" ~name:"Submit a new entry";
                  newline;
                  heading `H2 "Entries:";
                  text book#print;
                ]
              |> M.respond_gemtext);
          M.route
            Path.(~/"submit")
            (fun req ->
              match Request.query req with
              | None -> M.respond Status.input "Enter your message"
              | Some msg ->
                  book#add_entry ~addr:(Request.ip req) msg;
                  M.respond Status.redirect_temp "/");
        ]
      |> M.run ~certs:(Single cert)
    end
