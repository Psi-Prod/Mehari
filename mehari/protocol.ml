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
  | SNIExtRequired
  | UserInfoNotAllowed
  | WrongHost
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
  | SNIExtRequired, SNIExtRequired
  | UserInfoNotAllowed, UserInfoNotAllowed
  | WrongHost, WrongHost
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
  | SNIExtRequired, _
  | UserInfoNotAllowed, _
  | WrongHost, _
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
  | SNIExtRequired -> "SNIExtRequired"
  | UserInfoNotAllowed -> "UserInfoNotAllowed"
  | WrongHost -> "WrongHost"
  | WrongPort -> "WrongPort"
  | WrongScheme -> "WrongScheme"

let check_sni = function
  | None -> Error SNIExtRequired
  | Some hostname -> Ok hostname

let check_utf8_encoding url =
  if String.is_valid_utf_8 url then Ok () else Error MalformedUTF8

let check_length url =
  let length = Bytes.of_string url |> Bytes.length in
  if length = 0 then Error EmptyURL
  else if length > 1024 then Error AboveMaxSize
  else Ok ()

let check_bom url =
  if
    String.get_utf_8_uchar url 0
    |> Uchar.utf_decode_uchar |> Uchar.equal Uchar.bom
  then Error BeginWithBOM
  else Ok ()

let check_scheme uri =
  match Uri.scheme uri with
  | None -> Error MissingScheme
  | Some scheme when scheme <> "gemini" -> Error WrongScheme
  | Some _ -> Ok ()

let check_user_info uri =
  match Uri.userinfo uri with
  | None -> Ok ()
  | Some _ -> Error UserInfoNotAllowed

let check_path uri =
  if Uri.path uri |> Filename.is_relative then Error RelativePath else Ok uri

let check_host uri certs =
  match Uri.host uri with
  | None | Some "" -> Error MissingHost
  | Some h -> (
      match Domain_name.of_string h with
      | Ok dn -> (
          match Domain_name.host dn with
          | Ok h ->
              if Certs.Private.supports_hostname certs h then Ok ()
              else Error WrongHost
          | Error _ -> Error NotADomainName)
      | Error _ -> Error NotADomainName)

let check_port uri port =
  match Uri.port uri with
  | None -> Ok ()
  | Some p when Int.equal port p -> Ok ()
  | Some _ -> Error WrongPort

let ( let* ) = Result.bind

let make_request ~client_ip ?hostname ~port ~verify_url_host ~tls_version
    ?client_cert ~client_request:input certs =
  let* sni = check_sni hostname in
  let* () = check_utf8_encoding input in
  let* () = check_length input in
  let* () = check_bom input in
  let uri = Uri.of_string input |> Uri.canonicalize in
  let* () = check_scheme uri in
  let* () = check_user_info uri in
  let* uri = check_path uri in
  let* () = if verify_url_host then check_host uri certs else Ok () in
  let* () = check_port uri port in
  Request.Private.make ?client_cert ~uri ~client_ip ~port ~sni ~tls_version ()
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
  | SNIExtRequired -> fmt "SNI extension to TLS is required"
  | UserInfoNotAllowed ->
      fmt "URL contains userinfo subcomponent which is not allowed"
  | WrongHost -> fmt "URL contains a foreign hostname"
  | WrongPort -> fmt "URL has an incorrect port number"
  | WrongScheme -> fmt {|URL scheme is not "gemini://"|}

let to_response err =
  let body = Format.asprintf "%a" pp_msg err in
  let status =
    match err with
    | AboveMaxSize | BeginWithBOM | EmptyURL | InvalidURL | MalformedUTF8
    | MissingHost | MissingScheme | NotADomainName | RelativePath
    | SNIExtRequired | UserInfoNotAllowed ->
        Response.Status.bad_request
    | WrongHost | WrongPort | WrongScheme ->
        Response.Status.proxy_request_refused
  in
  Response.respond status body
