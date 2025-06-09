open Cmdliner

let port =
  let doc =
    Arg.info ~doc:"The TCP port on which to listen for incoming connections."
      [ "port" ]
  in
  Mirage_runtime.register_arg Arg.(value & opt (some int) None doc)

module GeminiServer
    (FS : Mirage_kv.RO)
    (Keys : Mirage_kv.RO)
    (S : Tcpip.Stack.V4V6) =
struct
  module X509 = Tls_mirage.X509 (Keys)
  module Mehari_io = Mehari_mirage.Make (Mirage_ptime) (S) (Mirage_time)
  open Mehari
  open Lwt.Infix

  let guess_mime path =
    if Filename.check_suffix path ".gmi" then
      Mime.gemini ~charset:"utf-8" ~lang:[ "en" ] ()
    else
      Mime.from_filename ~charset:"utf-8" path
      |> Option.value ~default:Mime.app_octet_stream

  let not_found = Response.not_found Status.not_found "not found" |> Lwt.return

  let serve fs path =
    Lwt.catch
      (fun () ->
        FS.get fs (Mirage_kv.Key.v path) >>= function
        | Ok body ->
            Mehari_io.respond_body (Body.string body) (guess_mime path)
        | Error err ->
            Logs.info (fun log -> log "%a" FS.pp_error err);
            not_found)
      (fun _ -> not_found)

  let router fs request =
    match Request.target request with
    | "/" -> serve fs "index.gmi"
    | path -> serve fs path

  let start fs keys stack =
    let* cert = X509.certificate keys `Default in
    router fs |> Mehari_io.logger
    |> Mehari_io.run ?port:(Key_gen.port ()) ~certs:(Single cert) stack
end
