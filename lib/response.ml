type t = string

type _ status =
  | SlowDown : (int * int) -> string status
  | Success : (int * body) -> mime status
  | Other : int -> string status

and mime = { mime : string; charset : string option; lang : string option }
and body = Text of string | Gemtext of Gemtext.t

let string_of_mime m =
  m.mime
  ^ Option.fold m.charset ~none:"" ~some:(Printf.sprintf "; charset=%s")
  ^ Option.fold m.lang ~none:"" ~some:(Printf.sprintf "; lang=%s")

let text t = Text t
let gemtext g = Gemtext g
let string_of_body = function Text t -> t | Gemtext _ -> failwith "todo"

let validate code meta body =
  if Bytes.(of_string meta |> length) > 1024 then invalid_arg "too long header"
  else
    Option.fold body ~none:"" ~some:string_of_body
    |> Format.sprintf "%i %s\r\n%s" code meta

let to_string (type a) (s : a status) (info : a) =
  let code, (meta : string), body =
    match s with
    | SlowDown (code, n) -> (code, Int.to_string n, None)
    | Success (code, body) -> (code, string_of_mime info, Some body)
    | Other code -> (code, info, None)
  in
  validate code meta body

let make_mime ?charset ?lang ?(mime = "") () = { mime; charset; lang }
let empty_mime = make_mime ~mime:"" ()

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

let response status info = to_string status info
let respond status info = to_string status info |> Lwt.return
