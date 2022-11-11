type response = Gemtext of string | Text of string

module Header = struct
  type _ t =
    | SlowDown : (int * int) -> string t
    | Success : (int * string) -> meta_success t
    | Other : int -> string t

  and meta_success = {
    mime : string;
    charset : string option;
    lang : string option;
  }

  let validate code meta =
    if Bytes.(of_string meta |> length) < 1024 then
      invalid_arg "too long header"
    else Format.sprintf "%i %s\r\n" code meta

  let to_string (type a) (t : a t) (info : a) =
    let code, (meta : string) =
      match t with
      | SlowDown (code, n) -> (code, Int.to_string n)
      | Success (code, _) ->
          ( code,
            info.mime
            ^ Option.fold info.charset ~none:""
                ~some:(Printf.sprintf "; charset=%s")
            ^ Option.fold info.lang ~none:"" ~some:(Printf.sprintf "; lang=%s")
          )
      | Other code -> (code, info)
    in

    validate code meta
end

module Resp = struct
  open Header

  let default = { mime = ""; charset = None; lang = None }
  let input = Other 10
  let sensitive_input = Other 11
  let sucess body = Success (20, body)
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

let response status info = Header.to_string status info
