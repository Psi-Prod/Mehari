type t = string

type 'a status = int * 'a typ

and _ typ =
  | Success : body -> Mime.t typ
  | SlowDown : int -> string typ
  | Meta : string typ
  | MetaBody : body -> string typ

and body = Text of string | Gemtext of Gemtext.t

let text t = Text t
let gemtext g = Gemtext g
let lines l = String.concat "\n" l |> text

let page ~title body =
  gemtext Gemtext.[ heading `H1 title; text "\n"; text body ]

let string_of_body = function Text t -> t | Gemtext g -> Gemtext.to_string g

let validate code meta body =
  if Bytes.(of_string meta |> length) > 1024 then invalid_arg "too long header"
  else
    Option.fold body ~none:"" ~some:string_of_body
    |> Format.sprintf "%i %s\r\n%s" code meta

let to_string (type a) ((code, status) : a status) (m : a) =
  let meta, body =
    match status with
    | Success body -> (Mime.to_string m, Some body)
    | SlowDown n -> (Int.to_string n, None)
    | Meta -> (m, None)
    | MetaBody b -> (m, Some b)
  in
  validate code meta body

module Status = struct
  let input = (10, Meta)
  let sensitive_input = (11, Meta)
  let success body = (20, Success body)
  let redirect_temp = (30, Meta)
  let redirect_perm = (31, Meta)
  let temporary_failure = (40, Meta)
  let server_unavailable = (41, Meta)
  let cgi_error = (42, Meta)
  let proxy_error = (43, Meta)
  let slow_down n = (44, SlowDown n)
  let perm_failure = (50, Meta)
  let not_found = (51, Meta)
  let gone = (52, Meta)
  let proxy_request_refused = (53, Meta)
  let bad_request = (59, Meta)
  let client_cert_req = (60, Meta)
  let cert_not_authorised = (61, Meta)
  let cert_not_valid = (62, Meta)
  let code_of_status (c, _) = c
end

let response status info = to_string status info
let respond status info = to_string status info |> Lwt.return
let respond_body body = respond (Status.success body)

let respond_text txt =
  respond (Status.success (text txt)) (Mime.text_mime "plain")

let respond_gemtext g = respond (Status.success (gemtext g)) Mime.gemini
let raw_response code ~meta ~body = to_string (code, MetaBody (text body)) meta
let raw_respond code ~meta ~body = raw_response code ~meta ~body |> Lwt.return
