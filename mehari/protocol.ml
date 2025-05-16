type err =
  | AboveMaxSize
  | BeginWithBOM
  | EmptyURL
  | InvalidURL
  | MalformedUTF8
  | MissingHost
  | MissingScheme
  | NotADomainName
  | RelativePath
  | UserInfoNotAllowed
  | WrongPort
  | WrongScheme

let equal_err e e' =
  match (e, e') with
  | AboveMaxSize, AboveMaxSize
  | BeginWithBOM, BeginWithBOM
  | EmptyURL, EmptyURL
  | InvalidURL, InvalidURL
  | MalformedUTF8, MalformedUTF8
  | MissingHost, MissingHost
  | MissingScheme, MissingScheme
  | NotADomainName, NotADomainName
  | RelativePath, RelativePath
  | UserInfoNotAllowed, UserInfoNotAllowed
  | WrongPort, WrongPort
  | WrongScheme, WrongScheme ->
      true
  | AboveMaxSize, _
  | BeginWithBOM, _
  | EmptyURL, _
  | InvalidURL, _
  | MalformedUTF8, _
  | MissingHost, _
  | MissingScheme, _
  | NotADomainName, _
  | RelativePath, _
  | UserInfoNotAllowed, _
  | WrongPort, _
  | WrongScheme, _ ->
      false

let pp_err fmt err =
  Format.pp_print_string fmt
  @@
  match err with
  | AboveMaxSize -> "AboveMaxSize"
  | BeginWithBOM -> "BeginWithBOM"
  | EmptyURL -> "EmptyURL"
  | InvalidURL -> "InvalidURL"
  | MalformedUTF8 -> "MalformedUTF8"
  | MissingScheme -> "MissingScheme"
  | MissingHost -> "MissingHost"
  | NotADomainName -> "NotADomainName"
  | RelativePath -> "RelativePath"
  | UserInfoNotAllowed -> "UserInfoNotAllowed"
  | WrongPort -> "WrongPort"
  | WrongScheme -> "WrongScheme"

let check_request_is_utf8_encoded url =
  if String.is_valid_utf_8 url then Ok () else Error MalformedUTF8

let check_url_length url =
  let length = Bytes.of_string url |> Bytes.length in
  if length = 0 then Error EmptyURL
  else if length > 1024 then Error AboveMaxSize
  else Ok ()

let check_dont_begin_with_bom url =
  if
    String.get_utf_8_uchar url 0
    |> Uchar.utf_decode_uchar |> Uchar.equal Uchar.bom
  then Error BeginWithBOM
  else Ok ()

let check_gemini_scheme uri =
  match Uri.scheme uri with
  | None -> Error MissingScheme
  | Some scheme -> if scheme = "gemini" then Ok () else Error WrongScheme

let check_host uri =
  match Uri.host uri with
  | None | Some "" -> Error MissingHost
  | Some h -> (
      match Domain_name.of_string h with
      | Ok dn -> (
          match Domain_name.host dn with
          | Ok _ -> Ok h
          | Error _ -> (
              match Ipaddr.of_string h with
              | Ok _ -> Ok h
              | Error _ -> Error NotADomainName))
      | Error _ -> Error NotADomainName)

let check_no_user_info uri =
  match Uri.userinfo uri with
  | None -> Ok ()
  | Some _ -> Error UserInfoNotAllowed

let check_path uri =
  if Uri.path uri |> Filename.is_relative then Error RelativePath else Ok uri

let check_port uri port =
  match Uri.port uri with
  | None -> Ok ()
  | Some p -> if Int.equal port p then Ok () else Error WrongPort

let make_request ~client_ip ?hostname ~port ~tls_version ?client_cert
    ~client_request:input () =
  let ( let* ) = Result.bind in
  let* () = check_request_is_utf8_encoded input in
  let* () = check_url_length input in
  let* () = check_dont_begin_with_bom input in
  let uri = Uri.of_string input |> Uri.canonicalize in
  let* () = check_gemini_scheme uri in
  let* uri_hostname = check_host uri in
  let* () = check_no_user_info uri in
  let* uri = check_path uri in
  let* () = check_port uri port in
  let server_hostname =
    Option.map Domain_name.to_string hostname
    |> Option.value ~default:uri_hostname
  in
  let tls_version =
    match tls_version with
    | `TLS_1_0 | `TLS_1_1 ->
        assert false
        (* We explicitely don't support TLS version < 1.2 at server side. *)
    | (`TLS_1_2 | `TLS_1_3) as v -> v
  in
  Request.Private.make ?client_cert ~uri ~client_ip ~port ~server_hostname
    ~tls_version ()
  |> Result.ok

let pp_msg fmt =
  let fmt = Format.fprintf fmt in
  function
  | AboveMaxSize -> fmt "Request has a size higher than 1024 bytes"
  | BeginWithBOM -> fmt "The request begin with a U+FEFF byte order mark"
  | EmptyURL -> fmt "URL is empty"
  | InvalidURL -> fmt "invalid URL"
  | MalformedUTF8 -> fmt "URL contains non-UTF8 byte sequence"
  | MissingScheme -> fmt "URL has no scheme"
  | MissingHost -> fmt "The host URL subcomponent is required"
  | NotADomainName -> fmt "The host URL component is not a valid domain name"
  | RelativePath -> fmt "URL path is relative"
  | UserInfoNotAllowed ->
      fmt "URL contains userinfo subcomponent which is not allowed"
  | WrongPort -> fmt "URL has an incorrect port number"
  | WrongScheme -> fmt {|URL scheme is not "gemini://"|}

let to_response err =
  let body = Format.asprintf "%a" pp_msg err in
  let status =
    match err with
    | AboveMaxSize | BeginWithBOM | EmptyURL | InvalidURL | MalformedUTF8
    | MissingHost | MissingScheme | NotADomainName | RelativePath
    | UserInfoNotAllowed ->
        Response.Status.bad_request
    | WrongPort | WrongScheme -> Response.Status.proxy_request_refused
  in
  Response.respond status body
