module Blk = struct
  type t = Mkernel.Block.t

  let pagesize = Mkernel.Block.pagesize
  let read = Mkernel.Block.atomic_read
  let write = Mkernel.Block.atomic_write
end

module Fat = Mfat.Make (Blk)
module Rng = Mirage_crypto_rng.Fortuna

let load_certchains fs =
  let read_certchains = function
    | { Mfat.name; is_dir = true; _ } ->
        let open Result.Syntax in
        let decode filename decode =
          let path = Filename.concat name filename in
          if Fat.exists fs path then
            let* content = Fat.read fs path in
            decode content
          else Fmt.kstr (fun msg -> Error (`Msg msg)) "%s does not exist" path
        in
        let+ pk = decode "pk.pem" X509.Private_key.decode_pem
        and+ certs = decode "certs.pem" X509.Certificate.decode_pem_multiple in
        (certs, pk)
    | _ -> Error (`Msg "Not a directory")
  in
  match Fat.ls fs "" with
  | Ok entries ->
      List.filter_map (Fun.compose Result.to_option read_certchains) entries
  | Error _ -> []

open Mehari

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

let run () (cidrv4, gateway, ipv6) port =
  let devices =
    let open Mkernel in
    let rng =
      map (fun () -> Mirage_crypto_rng_mkernel.initialize (module Rng)) []
    and net = Mnet.stack ~name:"service" ?gateway ~ipv6 cidrv4
    and fs =
      map
        (fun blk () ->
          Fat.create blk
          |> Result.map_error (fun (`Msg msg) -> msg)
          |> Result.error_to_failure)
        [ block "certs" ]
    in
    [ rng; net; fs ]
  in
  Mkernel.run devices @@ fun rng (daemon, tcp, _) fs () ->
  Fun.protect ~finally:(fun () ->
      Mirage_crypto_rng_mkernel.kill rng;
      Mnet.kill daemon)
  @@ fun () ->
  let certs = load_certchains fs in
  Mehari_mirage.run ?port ~certs tcp (Logger.logger router)

open Cmdliner

let port =
  let doc = "The Gemini port" in
  let open Arg in
  value & opt (some int) None & info [ "p"; "port" ] ~doc ~docv:"PORT"

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let pp header k ppf fmt =
      let timestamp = Mirage_ptime.now () in
      Fmt.kpf k ppf
        ("[%a]%a[%s]: " ^^ fmt ^^ "\n%!")
        (Ptime.pp_human ()) timestamp Logs_fmt.pp_header (level, header)
        (Logs.Src.name src)
    in
    match level with
    | Logs.Debug -> k ()
    | _ -> msgf (fun ?header ?tags fmt -> pp header k ppf fmt)
  in
  { Logs.report }

let setup_logs =
  let setup level =
    Fmt.set_utf_8 Fmt.stdout true;
    Logs.set_level level;
    Logs.set_reporter (reporter Fmt.stdout)
  in
  Term.(const setup $ Logs_cli.level ~docs:"OUTPUT OPTIONS" ())

let term =
  let open Term in
  const run $ setup_logs $ Mnet_cli.setup $ port

let cmd =
  let info = Cmd.info "mehari" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
