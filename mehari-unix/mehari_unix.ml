open Lwt.Syntax
module Stack = Tcpip_stack_socket.V4V6
include Mehari.Mirage.Make (Pclock) (Mirage_kv_unix) (Stack)

let read_chunks ?(chunk_size = 16384) path =
  let+ ic = Lwt_io.open_file path ~mode:Input in
  let ended = ref false in
  Lwt_stream.from (fun () ->
      if !ended then Lwt.return_none
      else
        let* chunk = Lwt_io.read ~count:chunk_size ic in
        if String.length chunk = chunk_size then Lwt.return_some chunk
        else (
          ended := true;
          Lwt.return_some chunk))

let respond_document ?mime path =
  let open Mehari in
  if%lwt Lwt_unix.file_exists path then
    let mime = Option.value ~default:(from_filename path) mime in
    let* chunks = read_chunks path in
    respond_body (stream chunks) mime
  else respond not_found ""

let from_filename ?(lookup = `Ext) ?charset ?lang fname =
  match lookup with
  | `Ext -> Mehari.from_filename ?charset ?lang fname |> Lwt.return
  | `Content ->
      let+ content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
      Mehari.from_content ?charset ?lang content
      |> Option.value ~default:Mehari.gemini
  | `Both ->
      let+ content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
      Mehari.from_content ?charset ?lang content
      |> Option.value ~default:(Mehari.from_filename ?charset ?lang fname)

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
