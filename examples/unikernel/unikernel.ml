let src = Logs.Src.create "gemini-srv" ~doc:"Gemini server"

module Log = (val Logs.src_log src : Logs.LOG)

module GeminiServer
    (Random : Mirage_random.S)
    (FS : Mirage_kv.RO)
    (Keys : Mirage_kv.RO)
    (P : Mirage_clock.PCLOCK)
    (S : Tcpip.Stack.V4V6)
    (T : Mirage_time.S) =
struct
  module X509 = Tls_mirage.X509 (Keys) (P)
  module Mehari_io = Mehari_mirage.Make (P) (S) (T)
  open Lwt.Infix

  let guess_mime path =
    if Filename.check_suffix path ".gmi" then
      Mehari.gemini ~charset:"utf-8" ~lang:[ "en" ] ()
    else
      Mehari.from_filename ~charset:"utf-8" path
      |> Option.value ~default:Mehari.no_mime

  let not_found = Mehari_io.respond Mehari.not_found "not found"

  let serve fs path =
    Lwt.catch
      (fun () ->
        FS.get fs (Mirage_kv.Key.v path) >>= function
        | Ok body ->
            Mehari_io.respond_body (Mehari.string body) (guess_mime path)
        | Error err ->
            Log.info (fun log -> log "%a" FS.pp_error err);
            not_found)
      (fun _ -> not_found)

  let router fs request =
    match Mehari.target request with
    | "/" -> serve fs "index.gmi"
    | path -> serve fs path

  let start _ fs keys _ stack _ =
    X509.certificate keys `Default >>= fun cert ->
    router fs |> Mehari_io.logger
    |> Mehari_io.run ~port:(Key_gen.port ()) ~certchains:[ cert ] stack
end
