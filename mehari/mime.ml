open Lwt.Syntax

type t = { mime : string; charset : string option; lang : string list }

let make ?charset ?(lang = []) mime =
  {
    mime;
    charset =
      (match charset with
      | None when String.starts_with ~prefix:"text/" mime -> Some "utf-8"
      | _ -> None);
    lang;
  }

let database = Conan.Process.database ~tree:Conan_light.tree

let lookup_ext fname =
  Magic_mime.lookup ~default:"text/gemini" fname |> Lwt.return

let lookup_content fname =
  let* content = Lwt_io.with_file ~mode:Input fname Lwt_io.read in
  Lwt.return
  @@
  match Conan_string.run ~database content with
  | Ok meta -> Conan.Metadata.mime meta
  | Error _ -> None

let from_filename ?(lookup = `Ext) ?charset ?(lang = []) fname =
  let* mime =
    match lookup with
    | `Ext -> lookup_ext fname
    | `Content -> (
        match%lwt lookup_content fname with
        | None -> Lwt.return "text/gemini"
        | Some m -> Lwt.return m)
    | `Both -> (
        match%lwt lookup_content fname with
        | None -> lookup_ext fname
        | Some m -> Lwt.return m)
  in
  make mime ~charset ~lang |> Lwt.return

let empty = make ""
let text_mime text = make ("text/" ^ text)
let gemini = make "text/gemini"
let with_charset t c = { t with charset = Some c }
let with_lang t l = { t with lang = l }
let with_mime t mime = { t with mime }

let to_string t =
  t.mime
  ^ Option.fold t.charset ~none:"" ~some:(Printf.sprintf "; charset=%s")
  ^ match t.lang with [] -> "" | l -> "; lang=" ^ String.concat "," l
