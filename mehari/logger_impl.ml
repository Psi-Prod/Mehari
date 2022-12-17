module type S = sig
  module IO : Types.IO

  type addr
  type handler = addr Handler.Make(IO).t

  val set_level : Logs.level -> unit
  val logger : handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

module Make
    (Clock : Mirage_clock.PCLOCK) (IO : sig
      include Types.IO

      val finally : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
    end)
    (Addr : Types.ADDR) : S with module IO = IO and type addr = Addr.t = struct
  module IO = IO

  type addr = Addr.t
  type handler = addr Handler.Make(IO).t

  let src = Logs.Src.create "mehari.log"

  module Log = (val Logs.src_log src)

  let debug = Log.debug
  let info = Log.info
  let warning = Log.warn
  let error = Log.err
  let set_level lvl = Logs.Src.set_level src (Some lvl)

  let iter_backtrace f backtrace =
    String.split_on_char '\n' backtrace
    |> List.iter (function "" -> () | l -> f l)

  let logger handler req =
    IO.finally
      (fun () -> handler req)
      (fun resp ->
        Log.info (fun log ->
            log "%s %a"
              (Request.uri req |> Uri.path_and_query)
              Addr.pp (Request.ip req));
        IO.return resp)
      (fun exn ->
        let backtrace = Printexc.get_backtrace () in
        Log.warn (fun log -> log "Aborted by: %s" (Printexc.to_string exn));
        iter_backtrace
          (fun line -> Log.warn (fun log -> log "%s" line))
          backtrace;
        raise exn)
end
