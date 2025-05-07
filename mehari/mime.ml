type t = { mime : string; charset : string option; lang : string list }

let make ?charset = function
  | "" -> invalid_arg "Mehari.make_mime"
  | mime ->
      let charset =
        match charset with
        | None when String.starts_with ~prefix:"text/" mime -> Some "utf-8"
        | _ -> None
      in
      { mime; charset; lang = [] }

let gemini ?charset ?(lang = []) () =
  { (make ?charset "text/gemini") with lang }

let text text = make ("text/" ^ text)
let app_octet_stream = make "application/octet-stream"
let plaintext = text "plain"
let with_charset t c = { t with charset = Some c }

let from_filename ?charset fname =
  match Conan_bindings.Extensions.(Map.find_opt fname map) with
  | None -> None
  | Some [] -> assert false
  | Some (m :: _) -> make m ~charset |> Option.some

let from_content ?charset ~tree content =
  match Conan_string.run ~database:(Conan.Process.database ~tree) content with
  | Ok meta -> Conan.Metadata.mime meta |> Option.map (make ?charset)
  | Error _ -> None

let to_string { mime; charset; lang } =
  let charset =
    match charset with
    | None -> ""
    | Some cs -> Printf.sprintf "; charset=%s" cs
  in
  let lang =
    if mime = "text/gemini" then "; lang=" ^ String.concat "," lang else ""
  in
  mime ^ charset ^ lang
