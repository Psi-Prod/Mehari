open Mehari
open Lwt.Infix
open Lwt.Syntax

let respond_document ?(mime = Mime.app_octet_stream) path =
  Lwt_unix.file_exists path >>= function
  | true ->
      let+ content = Lwt_io.with_file ~mode:Input path Lwt_io.read in
      Response.body (Body.string content) mime
  | false -> Response.respond Status.not_found "" |> Lwt.return

include Private.Static.Make (struct
  module IO = Lwt

  type path = string

  let kind path =
    Lwt.catch
      (fun () ->
        Lwt_unix.stat path >|= function
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
