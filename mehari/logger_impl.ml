module type S = sig
  val set_level : Logs.level -> unit
  val logger : Handler.t -> Handler.t
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

module Make (Clock : Mirage_clock.PCLOCK) : S = struct
  let src = Logs.Src.create "mehari.log"

  module Log = (val Logs.src_log src)

  let debug = Log.debug
  let info = Log.info
  let warning = Log.warn
  let error = Log.err
  let set_level lvl = Logs.Src.set_level src (Some lvl)

  let iter_backtrace f backtrace =
    backtrace |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
    |> List.iter f

  let logger handler req =
    Lwt.try_bind
      (fun () -> handler req)
      (fun resp ->
        Log.info (fun log ->
            log "%s %s"
              (Request.uri req |> Uri.path_and_query)
              (Request.ip req |> Ipaddr.to_string));
        Lwt.return resp)
      (fun exn ->
        let backtrace = Printexc.get_backtrace () in
        Log.warn (fun log -> log "Aborted by: %s" (Printexc.to_string exn));
        iter_backtrace
          (fun line -> Log.warn (fun log -> log "%s" line))
          backtrace;
        Lwt.fail exn)
end
