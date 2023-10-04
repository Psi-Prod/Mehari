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

module M = Mehari_lwt_unix
open Lwt.Syntax

let main () =
  let* certchains = Common.Lwt.load_certchains () in
  M.router
    [
      M.route "/" (fun _ ->
          Mehari.Gemtext.
            [
              heading `H1 "Guestbook";
              newline;
              link "/submit" ~name:"Submit a new entry";
              newline;
              heading `H2 "Entries:";
              text book#print;
            ]
          |> M.respond_gemtext);
      M.route "/submit" (fun req ->
          match Mehari.query req with
          | None -> M.respond Mehari.input "Enter your message"
          | Some msg ->
              book#add_entry ~addr:(Mehari.ip req) msg;
              M.respond Mehari.redirect_temp "/");
    ]
  |> M.run_lwt ~certchains

let () = Lwt_main.run (main ())
