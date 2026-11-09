module Rng = Mirage_crypto_rng.Fortuna

module Fat = Mfat_bos.Make (struct
  type t = Mkernel.Block.t

  let pagesize = Mkernel.Block.sector_size
  let read = Mkernel.Block.atomic_read
  let write = Mkernel.Block.atomic_write
end)

let load_certchains fs =
  let open Result.Syntax in
  let read_certchains path =
    let from_filename fname decode =
      let* content = Fat.File.read fs Fpath.(path / fname) in
      decode content
    in
    let+ key = from_filename "key.pem" X509.Private_key.decode_pem
    and+ certs =
      from_filename "cert.pem" X509.Certificate.decode_pem_multiple
    in
    (certs, key)
  in
  let* entries = Fat.Dir.contents fs (Fpath.v "/") in
  match entries with
  | [] -> Error (`Msg "fat image doesn't contains any certchains")
  | paths ->
      let+ certchains =
        List.fold_left
          (fun acc path ->
            let+ acc and* certchain = read_certchains path in
            certchain :: acc)
          (Ok []) paths
      in
      List.rev certchains

let book =
  object
    val mutable entries = []

    method add_entry ~addr msg =
      entries <- (Mirage_ptime.now (), addr, msg) :: entries

    method print =
      let buf = Buffer.create 101 in
      List.iter
        (fun (ptime, addr, msg) ->
          let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime in
          Fmt.kstr (Buffer.add_string buf) "%i-%i-%i %i:%i:%i - %a: %s\n" y m d
            hh mm ss Ipaddr.pp addr (Uri.pct_decode msg))
        entries;
      Buffer.contents buf
  end

let router =
  let open Mehari in
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

let run _quiet stack port =
  let rng =
    Mkernel.map
      (fun () -> Mirage_crypto_rng_mkernel.(initialize (module Pfortuna)))
      []
  and certchains =
    Mkernel.map
      (fun blk () ->
        match Result.bind (Fat.create blk) load_certchains with
        | Ok certs -> certs
        | Error (`Msg msg) -> failwith msg)
      [ Mkernel.block "certs" ]
  in
  Mkernel.run [ rng; stack; certchains ] @@ fun rng (daemon, tcp, _) certs () ->
  let finally () =
    Mirage_crypto_rng_mkernel.kill rng;
    Mnet.kill daemon
  in
  Fun.protect ~finally @@ fun () ->
  Mehari_mirage.run ?port ~certs tcp (Mehari.Logger.logger router)

open Cmdliner

let port =
  let doc = "The Gemini port" in
  let open Arg in
  value & opt (some int) None & info [ "p"; "port" ] ~doc ~docv:"PORT"

let term =
  let open Term in
  const run $ Mnet_cli.setup_logs $ Mnet_cli.setup "service" $ port

let cmd =
  let info = Cmd.info "mehari" in
  Cmd.v info term

let () = Cmd.(exit (eval cmd))
