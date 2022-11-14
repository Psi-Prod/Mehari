type t = string

type _ status =
  | Redirect : int -> Uri.t status
  | Success : (int * body) -> Mime.t status
  | SlowDown : (int * int) -> string status
  | Other : int -> string status

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

let to_string (type a) (s : a status) (x : a) =
  let code, meta, body =
    match s with
    | Redirect code -> (code, Uri.to_string x, None)
    | Success (code, body) -> (code, Mime.to_string x, Some body)
    | SlowDown (code, n) -> (code, Int.to_string n, None)
    | Other code -> (code, x, None)
  in
  validate code meta body

module Status = struct
  let input = Other 10
  let sensitive_input = Other 11
  let success body = Success (20, body)
  let redirect_temp = Other 30
  let redirect_permanent = Other 31
  let temporary_failure = Other 40
  let server_unavailable = Other 41
  let cgi_error = Other 42
  let proxy_error = Other 43
  let slow_down n = SlowDown (44, n)
  let permanent_failure = Other 50
  let not_found = Other 51
  let gone = Other 52
  let proxy_request_refused = Other 53
  let bad_request = Other 59
  let client_certificate_required = Other 60
  let certificate_not_authorised = Other 61
  let certificate_not_valid = Other 62
end

open Lwt.Syntax

let response status info = to_string status info
let respond status info = to_string status info |> Lwt.return

let respond_text txt =
  respond (Status.success (text txt)) (Mime.make ~mime:"text/plain" ())

let respond_gemtext g = respond (Status.success (gemtext g)) Mime.gemini

let respond_document ?mime path =
  if%lwt Lwt_unix.file_exists path then
    let mime = Option.value mime ~default:Mime.gemini in
    let* content = Lwt_io.with_file ~mode:Input path Lwt_io.read in
    respond (Status.success (text content)) mime
  else respond Status.not_found ""

open Lwt.Syntax

let directory_listing path =
  let* exist = Lwt_unix.file_exists path in
  if exist || Sys.is_directory path then
    let dirs = Lwt_unix.files_of_directory path in
    let* links =
      Lwt_stream.fold
        (fun p acc -> Gemtext.link (Filename.concat path p) :: acc)
        dirs []
    in
    respond_gemtext (Gemtext.[ heading `H1 path; text "\n" ] @ links)
  else respond Status.not_found ""
