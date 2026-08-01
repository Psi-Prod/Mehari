type t = { mime : string; charset : string option; lang : string list }

let make ?charset = function
  | "" -> invalid_arg "Mehari.Mime.make"
  | mime ->
      let charset =
        match charset with
        | None when String.starts_with ~prefix:"text/" mime -> Some "utf-8"
        | c -> c
      in
      { mime; charset; lang = [] }

let gemini ?charset ?(lang = []) () =
  { (make ?charset "text/gemini") with lang }

let text text = make ("text/" ^ text)
let app_octet_stream = make "application/octet-stream"
let plaintext = text "plain"
let with_charset c t = { t with charset = Some c }

let from_filename ?charset fname =
  match Filename.extension fname with
  | "" -> None
  | dot_ext -> begin
      let ext = String.sub dot_ext 1 (String.length dot_ext - 1) in
      match Conan_bindings.Extensions.(Map.find_opt ext map) with
      | None -> None
      | Some [] -> assert false
      | Some (m :: _) -> Some (make ?charset m)
    end

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
    match (mime, lang) with
    | "text/gemini", [] -> ""
    | "text/gemini", [ _ ] -> "; lang=" ^ String.concat "," lang
    | "text/gemini", _ :: _ :: _ -> "; lang=\"" ^ String.concat "," lang ^ "\""
    | _ -> ""
  in
  mime ^ charset ^ lang

let equal m m' =
  String.equal m.mime m'.mime
  && Option.equal String.equal m.charset m'.charset
  && List.equal String.equal m.lang m'.lang
