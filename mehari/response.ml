type t = Immediate of string | Stream of string Lwt_stream.t

type 'a status = int * 'a typ

and _ typ =
  | Success : body -> Mime.t typ
  | SlowDown : int -> string typ
  | Meta : string typ

and body =
  | Text of string
  | Gemtext of Gemtext.t
  | Stream of string Lwt_stream.t

let text t = Text t
let gemtext g = Gemtext g
let lines l = String.concat "\n" l |> text
let stream stream = Stream stream

let page ~title body =
  gemtext Gemtext.[ heading `H1 title; text "\n"; text body ]

let validate code meta body =
  if Bytes.(of_string meta |> length) > 1024 then invalid_arg "too long header"
  else
    let meta = Printf.sprintf "%i %s\r\n" code meta in
    match body with
    | None -> Immediate meta
    | Some (Text t) -> Immediate (meta ^ t)
    | Some (Gemtext g) -> Immediate (meta ^ Gemtext.to_string g)
    | Some (Stream body) -> Stream Lwt_stream.(append (of_list [ meta ]) body)

let to_response (type a) ((code, status) : a status) (m : a) =
  let meta, body =
    match status with
    | Success body -> (Mime.to_string m, Some body)
    | SlowDown n -> (Int.to_string n, None)
    | Meta -> (m, None)
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
end

let response status info = to_response status info
let respond status info = to_response status info |> Lwt.return
let respond_body body = respond (Status.success body)

let respond_text txt =
  respond (Status.success (text txt)) (Mime.text_mime "plain")

let respond_gemtext g = respond (Status.success (gemtext g)) Mime.gemini