module type S = sig
  module IO : Types.IO

  type addr
  type handler = addr Handler.Make(IO).t
  type clock

  val set_level : Logs.level -> unit
  val logger : clock -> handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

module Make
    (Clock : Types.PCLOCK) (IO : sig
      include Types.IO

      val finally : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
    end)
    (Addr : Types.ADDR) :
  S with module IO = IO and type addr = Addr.t and type clock = Clock.t = struct
  module IO = IO

  type addr = Addr.t
  type handler = addr Handler.Make(IO).t
  type clock = Clock.t

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

  let now clock = Clock.now_d_ps clock |> Ptime.v |> Ptime.to_float_s

  let logger clock handler req =
    let start = now clock in
    IO.finally
      (fun () -> handler req)
      (fun resp ->
        Log.info (fun log ->
            log "Serve '%s' %a"
              (Request.uri req |> Uri.path_and_query)
              Addr.pp (Request.ip req));
        (match resp.Response.status with
        | None -> ()
        | Some code ->
            let elapsed = now clock -. start in
            Log.info (fun log -> log "%i in %f µs" code (elapsed *. 1e6)));
        IO.return resp)
      (fun exn ->
        let backtrace = Printexc.get_backtrace () in
        Log.warn (fun log -> log "Aborted by: %s" (Printexc.to_string exn));
        iter_backtrace
          (fun line -> Log.warn (fun log -> log "%s" line))
          backtrace;
        raise exn)
end
