type err =
  | Above_max_size
  | Begin_with_BOM
  | Empty_URL
  | Invalid_client_cert of X509.Validation.ca_error
  | Invalid_domain_name
  | Invalid_URL
  | Malformed_UTF8
  | Missing_host
  | Missing_scheme
  | Relative_path
  | User_info_not_allowed
  | Wrong_port
  | Wrong_scheme

let equal_err e e' =
  match (e, e') with
  | Above_max_size, Above_max_size
  | Begin_with_BOM, Begin_with_BOM
  | Invalid_client_cert _, Invalid_client_cert _
  | Empty_URL, Empty_URL
  | Invalid_URL, Invalid_URL
  | Malformed_UTF8, Malformed_UTF8
  | Missing_host, Missing_host
  | Missing_scheme, Missing_scheme
  | Invalid_domain_name, Invalid_domain_name
  | Relative_path, Relative_path
  | User_info_not_allowed, User_info_not_allowed
  | Wrong_port, Wrong_port
  | Wrong_scheme, Wrong_scheme ->
      true
  | Above_max_size, _
  | Begin_with_BOM, _
  | Empty_URL, _
  | Invalid_client_cert _, _
  | Invalid_domain_name, _
  | Invalid_URL, _
  | Malformed_UTF8, _
  | Missing_host, _
  | Missing_scheme, _
  | Relative_path, _
  | User_info_not_allowed, _
  | Wrong_port, _
  | Wrong_scheme, _ ->
      false

let pp_err ppf err =
  let open Format in
  match err with
  | Above_max_size -> pp_print_string ppf "Above_max_size"
  | Begin_with_BOM -> pp_print_string ppf "Begin_with_BOM"
  | Empty_URL -> pp_print_string ppf "Empty_URL"
  | Invalid_client_cert ca_err ->
      fprintf ppf {|Invalid_client_cert "%a"|} X509.Validation.pp_ca_error
        ca_err
  | Invalid_domain_name -> pp_print_string ppf "Invalid_domain_name"
  | Invalid_URL -> pp_print_string ppf "Invalid_URL"
  | Malformed_UTF8 -> pp_print_string ppf "Malformed_UTF8"
  | Missing_scheme -> pp_print_string ppf "Missing_scheme"
  | Missing_host -> pp_print_string ppf "Missing_host"
  | Relative_path -> pp_print_string ppf "Relative_path"
  | User_info_not_allowed -> pp_print_string ppf "User_info_not_allowed"
  | Wrong_port -> pp_print_string ppf "Wrong_port"
  | Wrong_scheme -> pp_print_string ppf "Wrong_scheme"

let check_utf8_encoded url =
  if String.is_valid_utf_8 url then Ok () else Error Malformed_UTF8

let check_url_length url =
  let length = Bytes.of_string url |> Bytes.length in
  if length = 0 then Error Empty_URL
  else if length > 1024 then Error Above_max_size
  else Ok ()

let check_begin_bom url =
  if
    String.get_utf_8_uchar url 0
    |> Uchar.utf_decode_uchar |> Uchar.equal Uchar.bom
  then Error Begin_with_BOM
  else Ok ()

let check_gemini_scheme uri =
  match Uri.scheme uri with
  | None -> Error Missing_scheme
  | Some scheme -> if scheme = "gemini" then Ok () else Error Wrong_scheme

let check_host uri =
  match Uri.host uri with
  | None | Some "" -> Error Missing_host
  | Some h ->
      begin match Domain_name.of_string h with
      | Ok dn ->
          begin match Domain_name.host dn with
          | Ok host -> Ok (Request.Private.Domain_name host)
          | Error _ ->
              begin match Ipaddr.of_string h with
              | Ok ip -> Ok (Ip_addr ip)
              | Error _ -> Error Invalid_domain_name
              end
          end
      | Error _ -> Error Invalid_domain_name
      end

let check_no_user_info uri =
  match Uri.userinfo uri with
  | None -> Ok ()
  | Some _ -> Error User_info_not_allowed

let check_path uri =
  if Uri.path uri |> Filename.is_relative then Error Relative_path else Ok uri

let check_port uri port =
  match Uri.port uri with
  | None -> Ok ()
  | Some p -> if Int.equal port p then Ok () else Error Wrong_port

let check_client_cert_validity ~now = function
  | None -> Ok ()
  | Some cert ->
      X509.Validation.valid_ca ~time:now cert
      |> Result.map_error (fun ca_err -> Invalid_client_cert ca_err)

let make_request ~client ?hostname ~port ~tls_version ?client_cert
    ~client_request ~now () =
  let open Result.Syntax in
  let* () = check_utf8_encoded client_request in
  let* () = check_url_length client_request in
  let* () = check_begin_bom client_request in
  let uri = Uri.of_string client_request |> Uri.canonicalize in
  let* () = check_gemini_scheme uri in
  let* requested_hostname = check_host uri in
  let* () = check_no_user_info uri in
  let* uri = check_path uri in
  let* () = check_port uri port in
  let* () = check_client_cert_validity ~now client_cert in
  let server_hostname =
    match hostname with
    | None -> requested_hostname
    | Some d -> Request.Private.Domain_name d
  in
  let tls_version =
    match tls_version with
    | `TLS_1_0 | `TLS_1_1 ->
        assert false
        (* We explicitely don't support TLS version < 1.2 at server side. *)
    | (`TLS_1_2 | `TLS_1_3) as v -> v
  in
  Request.Private.make ?client_cert ~uri ~client ~port ~server_hostname
    ~tls_version ()
  |> Result.ok

let pp_msg ppf =
  let open Format in
  function
  | Above_max_size ->
      pp_print_string ppf "Request has a size higher than 1024 bytes"
  | Begin_with_BOM ->
      pp_print_string ppf "The request begin with a U+FEFF byte order mark"
  | Empty_URL -> pp_print_string ppf "URL is empty"
  | Invalid_client_cert ca_err -> X509.Validation.pp_ca_error ppf ca_err
  | Invalid_domain_name ->
      pp_print_string ppf "The host URL component is not a valid domain name"
  | Invalid_URL -> pp_print_string ppf "invalid URL"
  | Malformed_UTF8 -> pp_print_string ppf "URL contains non-UTF8 byte sequence"
  | Missing_scheme -> pp_print_string ppf "URL has no scheme"
  | Missing_host -> pp_print_string ppf "The host URL subcomponent is required"
  | Relative_path -> pp_print_string ppf "URL path is relative"
  | User_info_not_allowed ->
      pp_print_string ppf
        "URL contains userinfo subcomponent which is not allowed"
  | Wrong_port -> pp_print_string ppf "URL has an incorrect port number"
  | Wrong_scheme -> pp_print_string ppf {|URL scheme is not "gemini://"|}

let to_response err =
  let status =
    match err with
    | Above_max_size | Begin_with_BOM | Empty_URL | Invalid_URL | Malformed_UTF8
    | Missing_host | Missing_scheme | Invalid_domain_name | Relative_path
    | User_info_not_allowed ->
        Response.Status.bad_request
    | Invalid_client_cert _ -> Response.Status.cert_not_valid
    | Wrong_port | Wrong_scheme -> Response.Status.proxy_request_refused
  in
  let error_msg = Format.asprintf "%a" pp_msg err in
  Response.respond status error_msg
