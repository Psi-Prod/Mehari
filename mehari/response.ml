type t = string

type 'a status = int * 'a typ

and _ typ =
  | Success : body -> Mime.t typ
  | SlowDown : int -> string typ
  | Other : string typ

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
    | Other -> (m, None)
  in
  validate code meta body

module Status = struct
  let input = (10, Other)
  let sensitive_input = (11, Other)
  let success body = (20, Success body)
  let redirect_temp = (30, Other)
  let redirect_permanent = (31, Other)
  let temporary_failure = (40, Other)
  let server_unavailable = (41, Other)
  let cgi_error = (42, Other)
  let proxy_error = (43, Other)
  let slow_down n = (44, SlowDown n)
  let permanent_failure = (50, Other)
  let not_found = (51, Other)
  let gone = (52, Other)
  let proxy_request_refused = (53, Other)
  let bad_request = (59, Other)
  let client_certificate_required = (60, Other)
  let certificate_not_authorised = (61, Other)
  let certificate_not_valid = (62, Other)
end

let response status info = to_string status info
let respond status info = to_string status info |> Lwt.return
let respond_body body = respond (Status.success body)

let respond_text txt =
  respond (Status.success (text txt)) (Mime.text_mime "plain")

let respond_gemtext g = respond (Status.success (gemtext g)) Mime.gemini
