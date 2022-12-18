type t = { mime : string; charset : string option; lang : string list }

let make_mime ?charset ?(lang = []) mime =
  {
    mime;
    charset =
      (match charset with
      | None when String.starts_with ~prefix:"text/" mime -> Some "utf-8"
      | _ -> None);
    lang;
  }

let empty = make_mime ""
let gemini = make_mime "text/gemini"
let text_mime text = make_mime ("text/" ^ text)
let plaintext = text_mime "plain"
let with_charset t c = { t with charset = Some c }

let from_filename ?charset ?(lang = []) fname =
  let mime = Magic_mime.lookup ~default:"text/gemini" fname in
  make_mime mime ~charset ~lang

let database = Conan.Process.database ~tree:Conan_light.tree

let from_content ?charset ?lang content =
  match Conan_string.run ~database content with
  | Ok meta -> Conan.Metadata.mime meta |> Option.map (make_mime ?charset ?lang)
  | Error _ -> None

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
