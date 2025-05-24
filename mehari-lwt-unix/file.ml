open Mehari
open Lwt.Infix
open Lwt.Syntax

(* TODO: true lazyness (is it even possible?) *)
let rec unfold f u () =
  f u >>= function
  | None -> Lwt.return Seq.Nil
  | Some (x, u') ->
      let+ xs = unfold f u' () in
      Seq.Cons (x, fun () -> xs)

let read_chunks path =
  let+ ic = Lwt_io.open_file path ~mode:Input in
  unfold
    (fun ended ->
      if ended then Lwt_io.close ic >|= fun () -> None
      else
        let+ chunk = Lwt_io.read ~count:4096 ic in
        if String.length chunk = 4096 then Some (chunk, false)
        else Some (chunk, true))
    false

let respond_document ?(mime = Mehari.Mime.app_octet_stream) path =
  Lwt_unix.file_exists path >>= function
  | true ->
      let* chunks = read_chunks path in
      let+ cs = chunks () in
      Response.body (Body.seq (fun () -> cs)) mime
  | false -> Response.respond Status.not_found "" |> Lwt.return

include Private.Static.Make (struct
  module IO = Lwt

  type path = string

  let kind path =
    Lwt.catch
      (fun () ->
        Lwt_unix.lstat path >|= function
        | { st_kind = S_REG; _ } -> `Regular_file
        | { st_kind = S_DIR; _ } -> `Directory
        | _ -> `Other)
      (function Unix.Unix_error _ -> Lwt.return `Other | exn -> raise exn)

  let exists = Lwt_unix.file_exists
  let read path = Lwt_unix.files_of_directory path |> Lwt_stream.to_list
  let concat = Filename.concat
  let respond_document = respond_document

  let pp_io_err fmt = function
    | Unix.Unix_error (err, fun_name, _) ->
        Format.fprintf fmt "Unix_error %S: %s" fun_name (Unix.error_message err)
    | exn -> raise exn
end)
