module type S = sig
  val init : Logs.level -> unit
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

  let init lvl =
    Logs.Src.set_level src (Some lvl);
    Logs.set_reporter (Logs.reporter ())

  let now () = Clock.now_d_ps () |> fst

  let timestamp =
    Logs.Tag.def "timestamp" (fun fmt () -> Format.fprintf fmt "%i" (now ()))

  let level = Logs.Tag.def "level" (fun fmt -> Format.fprintf fmt "%s")

  let pp_lvl l =
    Option.fold l ~none:"" ~some:(function
      | Logs.App -> "     "
      | Error -> "ERROR"
      | Warning -> " WARN"
      | Info -> " INFO"
      | Debug -> "DEBUG")

  let iter_backtrace f backtrace =
    backtrace |> String.split_on_char '\n'
    |> List.filter (( <> ) "")
    |> List.iter f

  let logger handler req =
    Lwt.try_bind
      (fun () -> handler req)
      (fun resp ->
        Log.info (fun log ->
            let tags =
              Logs.Tag.(
                empty |> add timestamp ()
                |> add level (Logs.Src.level src |> pp_lvl))
            in
            log ~tags "%s %s"
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

  (* let forward (dest : _ Logs.log) k =
     dest (fun log ->
         k (fun fmt_args ->
             log ~tags:Logs.Tag.(empty |> add timestamp ()) fmt_args)) *)
end
