type t = { mime : string; charset : string option; lang : string option }

let make ?charset ?lang ?(mime = "") () = { mime; charset; lang }
let empty = make ~mime:"" ()
let text_mime text = make ~mime:("text/" ^ text) ()
let gemini = make ~mime:"text/gemini" ()
let with_charset t c = { t with charset = Some c }
let with_lang t l = { t with lang = Some l }
let with_mime t mime = { t with mime }

let to_string t =
  t.mime
  ^ Option.fold t.charset ~none:"" ~some:(Printf.sprintf "; charset=%s")
  ^ Option.fold t.lang ~none:"" ~some:(Printf.sprintf "; lang=%s")
