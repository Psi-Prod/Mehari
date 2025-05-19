type err =
  | AboveMaxSize
  | BeginWithBOM
  | ClientCertificateNotValid of X509.Validation.ca_error
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
  | ClientCertificateNotValid _, ClientCertificateNotValid _
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
  | ClientCertificateNotValid _, _
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

let pp_err ppf err =
  let open Format in
  match err with
  | AboveMaxSize -> pp_print_string ppf "AboveMaxSize"
  | BeginWithBOM -> pp_print_string ppf "BeginWithBOM"
  | ClientCertificateNotValid ca_err ->
      fprintf ppf {|ClientCertificateNotValid "%a"|} X509.Validation.pp_ca_error
        ca_err
  | EmptyURL -> pp_print_string ppf "EmptyURL"
  | InvalidURL -> pp_print_string ppf "InvalidURL"
  | MalformedUTF8 -> pp_print_string ppf "MalformedUTF8"
  | MissingScheme -> pp_print_string ppf "MissingScheme"
  | MissingHost -> pp_print_string ppf "MissingHost"
  | NotADomainName -> pp_print_string ppf "NotADomainName"
  | RelativePath -> pp_print_string ppf "RelativePath"
  | UserInfoNotAllowed -> pp_print_string ppf "UserInfoNotAllowed"
  | WrongPort -> pp_print_string ppf "WrongPort"
  | WrongScheme -> pp_print_string ppf "WrongScheme"

let check_utf8_encoded url =
  if String.is_valid_utf_8 url then Ok () else Error MalformedUTF8

let check_url_length url =
  let length = Bytes.of_string url |> Bytes.length in
  if length = 0 then Error EmptyURL
  else if length > 1024 then Error AboveMaxSize
  else Ok ()

let check_begin_bom url =
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

let check_client_cert_validity ~now = function
  | None -> Ok ()
  | Some cert ->
      X509.Validation.valid_ca ~time:now cert
      |> Result.map_error (fun ca_err -> ClientCertificateNotValid ca_err)

let make_request ~client_ip ~client_port ?hostname ~port ~tls_version
    ?client_cert ~client_request ~now () =
  let ( let* ) = Result.bind in
  let* () = check_utf8_encoded client_request in
  let* () = check_url_length client_request in
  let* () = check_begin_bom client_request in
  let uri = Uri.of_string client_request |> Uri.canonicalize in
  let* () = check_gemini_scheme uri in
  let* uri_hostname = check_host uri in
  let* () = check_no_user_info uri in
  let* uri = check_path uri in
  let* () = check_port uri port in
  let* () = check_client_cert_validity ~now client_cert in
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
  Request.Private.make ?client_cert ~uri ~client_ip ~client_port ~port
    ~server_hostname ~tls_version ()
  |> Result.ok

let pp_msg ppf =
  let open Format in
  function
  | AboveMaxSize ->
      pp_print_string ppf "Request has a size higher than 1024 bytes"
  | BeginWithBOM ->
      pp_print_string ppf "The request begin with a U+FEFF byte order mark"
  | ClientCertificateNotValid ca_err -> X509.Validation.pp_ca_error ppf ca_err
  | EmptyURL -> pp_print_string ppf "URL is empty"
  | InvalidURL -> pp_print_string ppf "invalid URL"
  | MalformedUTF8 -> pp_print_string ppf "URL contains non-UTF8 byte sequence"
  | MissingScheme -> pp_print_string ppf "URL has no scheme"
  | MissingHost -> pp_print_string ppf "The host URL subcomponent is required"
  | NotADomainName ->
      pp_print_string ppf "The host URL component is not a valid domain name"
  | RelativePath -> pp_print_string ppf "URL path is relative"
  | UserInfoNotAllowed ->
      pp_print_string ppf
        "URL contains userinfo subcomponent which is not allowed"
  | WrongPort -> pp_print_string ppf "URL has an incorrect port number"
  | WrongScheme -> pp_print_string ppf {|URL scheme is not "gemini://"|}

let to_response err =
  let status =
    match err with
    | AboveMaxSize | BeginWithBOM | EmptyURL | InvalidURL | MalformedUTF8
    | MissingHost | MissingScheme | NotADomainName | RelativePath
    | UserInfoNotAllowed ->
        Response.Status.bad_request
    | ClientCertificateNotValid _ -> Response.Status.cert_not_valid
    | WrongPort | WrongScheme -> Response.Status.proxy_request_refused
  in
  let error_msg = Format.asprintf "%a" pp_msg err in
  Response.respond status error_msg
