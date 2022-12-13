type entry = {
  timestamp : Unix.tm;
  addr : (Ipaddr.V4.t, Ipaddr.V6.t) Ipaddr.v4v6;
  message : string;
}

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
          Printf.sprintf "%i-%i-%i %i:%i:%i - %s: %s\n"
            (e.timestamp.tm_year + 1900)
            (e.timestamp.tm_mon + 1) e.timestamp.tm_mday e.timestamp.tm_hour
            e.timestamp.tm_min e.timestamp.tm_sec (Ipaddr.to_string e.addr)
            (Uri.pct_decode e.message)
          |> Buffer.add_string buf)
        entries;
      Buffer.contents buf
  end

module M = Mehari
module M_unix = Mehari_unix

let () =
  M_unix.router
    [
      M_unix.route "/" (fun _ ->
          let home =
            M.Gemtext.
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
          match M.query req with
          | None -> M_unix.respond M.input "Enter your message"
          | Some msg ->
              book#add_entry ~addr:(M.ip req) msg;
              M_unix.respond M.redirect_temp "/");
    ]
  |> M_unix.run
