module Logs_reporter = Mirage_logs.Make (Pclock)

let setup_logs f =
  (* We only want Mehari's logs. *)
  let console_threshold src =
    match Logs.Src.name src with "mehari.log" -> Logs.Info | _ -> Logs.Warning
  in
  Logs.set_level (Some Info);
  Logs_reporter.(create ~ring_size:20 ~console_threshold () |> run) f

let n = ref 0

let () =
  (fun () ->
    (fun _ ->
      incr n;
      Mehari_unix.info (fun log -> log "Request n°: %i" !n);
      Mehari.respond_text "Success")
    |> Mehari_unix.logger |> Mehari_unix.run_lwt)
  |> setup_logs |> Lwt_main.run