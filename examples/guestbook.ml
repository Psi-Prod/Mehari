type entry = {
  timestamp : Unix.tm;
  addr : (Ipaddr.V4.t, Ipaddr.V6.t) Ipaddr.v4v6;
  message : string;
}

let book =
  object
    val mutable entries = []

    method add_entry ~timestamp ~addr message =
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

let () =
  let open Mehari_unix in
  router
    [
      route "/" (fun _ ->
          let home =
            Gemtext.
              [
                heading `H1 "Guestbook";
                text "\n";
                link "/submit" ~name:"Submit a new entry";
                text "\n";
                text book#print;
              ]
          in
          respond_gemtext home);
      route "/submit" (fun req ->
          match uri req |> Uri.verbatim_query with
          | None -> respond input "Enter your message"
          | Some msg ->
              let timestamp = Unix.time () |> Unix.gmtime in
              book#add_entry ~timestamp ~addr:(ip req) msg;
              respond redirect_temp "/");
    ]
  |> run
