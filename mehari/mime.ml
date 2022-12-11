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

let empty = make ""
let text_mime text = make ("text/" ^ text)
let gemini = make "text/gemini"
let with_charset t c = { t with charset = Some c }

let to_string { mime; charset; lang } =
  let charset =
    Option.fold charset ~none:"" ~some:(Printf.sprintf "; charset=%s")
  in
  let lang =
    match lang with
    | [] -> ""
    | l when mime = "text/gemini" -> "; lang=" ^ String.concat "," l
    | _ -> ""
  in
  mime ^ charset ^ lang

let from_filename ?charset ?(lang = []) fname =
  let mime = Magic_mime.lookup ~default:"text/gemini" fname in
  make mime ~charset ~lang

let database = Conan.Process.database ~tree:Conan_light.tree

let from_content ?charset ?lang content =
  match Conan_string.run ~database content with
  | Ok meta -> Conan.Metadata.mime meta |> Option.map (make ?charset ?lang)
  | Error _ -> None
