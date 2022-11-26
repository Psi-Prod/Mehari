open Lwt.Syntax

include
  Mehari.Mirage.TempMake (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)

let respond_document ?mime path =
  let open Mehari in
  if%lwt Lwt_unix.file_exists path then
    let mime = Option.value ~default:(from_filename path) mime in
    let* content = Lwt_io.with_file ~mode:Input path Lwt_io.read in
    respond (success (text content)) mime
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

(*
include Mehari.Make (Pclock) (Mirage_kv_unix) (Tcpip_stack_socket.V4V6)
*)

(*let stack ~v4 ~v6 =
    Tcpip_stack_socket.V4V6.TCP.connect
      ~ipv4_only:(match v6 with None -> true | _ -> false)
      ~ipv6_only:false v4 v6

  let run_lwt ?port ?certchains ?v4 ?v6 callback =
    let* stack =
      stack
        ~v4:
          (match v4 with
          | Some ip -> Ipaddr.V4.Prefix.of_string_exn ip
          | None -> Ipaddr.V4.Prefix.loopback)
        ~v6:(Option.map Ipaddr.V6.Prefix.of_string_exn v6)
    in
    run_lwt ?port ?certchains stack callback

  let run ?port ?certchains ?v4 ?v6 callback =
    run_lwt ?port ?certchains ?v4 ?v6 callback |> Lwt_main.run*)

let run_lwt ?port ?certchains ?v4 ?v6:_ callback =
  run_lwt ?port ?certchains (Option.value ~default:"127.0.0.1" v4) callback

let run ?port ?certchains ?v4 ?v6 callback =
  run_lwt ?port ?certchains ?v4 ?v6 callback |> Lwt_main.run
