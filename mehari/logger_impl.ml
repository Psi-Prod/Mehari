module type S = sig
  module IO : Signatures.IO

  type clock
  type handler = Request.t -> Response.t IO.t

  val set_level : Logs.level -> unit
  val logger : clock -> handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

module Make
    (Clock : Signatures.PCLOCK)
    (IO : sig
      include Signatures.IO

      val finally : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
    end) : S with module IO = IO and type clock = Clock.t = struct
  module IO = IO

  type clock = Clock.t
  type handler = Request.t -> Response.t IO.t

  let src = Logs.Src.create "mehari.log"

  module Log = (val Logs.src_log src)

  let debug, info, warning, error = Log.(debug, info, warn, err)
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
              Ipaddr.pp (Request.ip req));
        let code = Response.status resp in
        let elapsed = now clock -. start in
        Log.info (fun log -> log "%i in %f µs" code (elapsed *. 1e6));
        IO.return resp)
      (fun exn ->
        let backtrace = Printexc.get_backtrace () in
        Log.warn (fun log -> log "Aborted by: %s" (Printexc.to_string exn));
        iter_backtrace
          (fun line -> Log.warn (fun log -> log "%s" line))
          backtrace;
        raise exn)
end
