open Lwt.Syntax
include Mehari_io

(** TODO: true lazyness (is it even possible?) *)
let rec unfold f u () =
  match%lwt f u with
  | None -> Lwt.return Seq.Nil
  | Some (x, u') ->
      let* xs = unfold f u' () in
      Lwt.return (Seq.Cons (x, fun () -> xs))

let read_chunks ?(chunk_size = 16384) path =
  let+ ic = Lwt_io.open_file path ~mode:Input in
  unfold
    (fun ended ->
      if ended then
        let* () = Lwt_io.close ic in
        Lwt.return_none
      else
        let* chunk = Lwt_io.read ~count:chunk_size ic in
        if String.length chunk = chunk_size then Lwt.return_some (chunk, false)
        else Lwt.return_some (chunk, true))
    false

let respond_document ?mime path =
  if%lwt Lwt_unix.file_exists path then
    let mime =
      match mime with
      | None -> Mehari.from_filename path |> Option.value ~default:Mehari.empty
      | Some m -> m
    in
    let* chunks = read_chunks path in
    let* cs = chunks () in
    respond_body (Mehari.seq (fun () -> cs)) mime
  else respond Mehari.not_found ""

let from_filename ?(lookup = `Ext) ?charset ?lang fname =
  match lookup with
  | `Ext -> Mehari.from_filename ?charset ?lang fname |> Lwt.return
  | `Content ->
      let+ content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
      Mehari.from_content ?charset ?lang content
  | `Both -> (
      let+ content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
      match Mehari.from_content ?charset ?lang content with
      | None -> Mehari.from_filename ?charset ?lang fname
      | Some m -> Some m)

let run_cgi = Cgi.run_cgi

let stack ~v4 ~v6 =
  let* tcp =
    Stack.TCP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6
  in
  let* udp =
    Stack.UDP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6
  in
  Stack.connect udp tcp

let run_lwt ?port ?certchains ?v4 ?v6 callback =
  let* stack =
    let v4 =
      match v4 with
      | Some ip -> Ipaddr.V4.Prefix.of_string_exn ip
      | None -> Ipaddr.V4.Prefix.loopback
    in
    stack ~v4 ~v6:(Option.map Ipaddr.V6.Prefix.of_string_exn v6)
  in
  run_lwt ?port ?certchains stack callback

let run ?port ?certchains ?v4 ?v6 callback =
  run_lwt ?port ?certchains ?v4 ?v6 callback |> Lwt_main.run
