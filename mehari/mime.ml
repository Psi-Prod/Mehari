type t = { mime : string; charset : string option; lang : string list }

let make_mime ?charset = function
  | "" -> raise (Invalid_argument "Mehari.make_mime")
  | mime ->
      {
        mime;
        charset =
          (match charset with
          | None when String.starts_with ~prefix:"text/" mime -> Some "utf-8"
          | _ -> None);
        lang = [];
      }

let gemini ?charset ?(lang = []) () =
  { (make_mime ?charset "text/gemini") with lang }

let text text = make_mime ("text/" ^ text)
let app_octet_stream = make_mime "application/octet-stream"
let plaintext = text "plain"
let with_charset t c = { t with charset = Some c }

let from_filename ?charset fname =
  match Magic_mime.lookup ~default:"" fname with
  | "" -> None
  | mime -> make_mime mime ~charset |> Option.some

let from_content ?charset ~tree content =
  match Conan_string.run ~database:(Conan.Process.database ~tree) content with
  | Ok meta -> Conan.Metadata.mime meta |> Option.map (make_mime ?charset)
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
