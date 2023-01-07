type request_err =
  | AboveMaxSize
  | BeginWithBOM
  | EmptyURL
  | InvalidURL
  | MalformedUTF8
  | MissingHost
  | MissingScheme
  | RelativePath
  | SNIExtRequired
  | UserInfoNotAllowed
  | WrongHost
  | WrongPort
  | WrongScheme

let check_sni epoch =
  Option.fold epoch.Tls.Core.own_name ~none:(Error SNIExtRequired)
    ~some:(fun d -> Domain_name.to_string d |> Result.ok)

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

let check_host uri epoch =
  match Uri.host uri with
  | None -> Error MissingHost
  | Some h -> (
      let hostnames =
        List.map Tls.Core.Cert.hostnames epoch.Tls.Core.own_certificate
        |> List.fold_left X509.Host.Set.union X509.Host.Set.empty
        |> X509.Host.Set.to_seq
        |> Seq.map (fun (_, d) -> Domain_name.to_string d)
      in
      match Seq.find (String.equal h) hostnames with
      | None -> Error WrongHost
      | Some _ -> Ok ())

let check_port uri port =
  match Uri.port uri with
  | None -> Ok ()
  | Some p when Int.equal port p -> Ok ()
  | Some _ -> Error WrongPort

let ( let+ ) x f = match x with Ok x -> f x | Error _ as err -> err

(* Perform some static check on client request *)
let make_request (type a) (module Addr : Types.ADDR with type t = a) ~port
    ~(addr : a) ~verify_url_host epoch input =
  let+ sni = check_sni epoch in
  let+ () = check_utf8_encoding input in
  let+ () = check_length input in
  let+ () = check_bom input in
  let uri = Uri.of_string input |> Uri.canonicalize in
  let+ () = check_scheme uri in
  let+ () = check_user_info uri in
  let+ uri = check_path uri in
  let+ () = if verify_url_host then check_host uri epoch else Ok () in
  let+ () = check_port uri port in
  Request.make
    (module Addr)
    ~uri ~addr ~port ~sni
    ~client_cert:(Option.to_list epoch.Tls.Core.peer_certificate)
  |> Result.ok

let pp_err fmt =
  let fmt = Format.fprintf fmt in
  function
  | AboveMaxSize -> fmt "Request has a size higher than 1024 bytes"
  | BeginWithBOM -> fmt "The request begin with a U+FEFF byte order mark"
  | EmptyURL -> fmt "URL is empty"
  | InvalidURL -> fmt "invalid URL"
  | MalformedUTF8 -> fmt "URL contains non-UTF8 byte sequence"
  | MissingScheme -> fmt "URL has no scheme"
  | MissingHost -> fmt "The host URL subcomponent is required"
  | RelativePath -> fmt "URL path is relative"
  | SNIExtRequired -> fmt "SNI extension to TLS is required"
  | UserInfoNotAllowed ->
      fmt "URL contains userinfo subcomponent which is not allowed"
  | WrongHost -> fmt "URL contains a foreign hostname"
  | WrongPort -> fmt "URL has an incorrect port number"
  | WrongScheme -> fmt {|URL scheme is not "gemini://"|}

let to_response err =
  let body = Format.asprintf "%a" pp_err err in
  let status =
    match err with
    | AboveMaxSize | BeginWithBOM | EmptyURL | InvalidURL | MalformedUTF8
    | MissingHost | MissingScheme | RelativePath | SNIExtRequired
    | UserInfoNotAllowed ->
        Response.Status.bad_request
    | WrongHost | WrongPort | WrongScheme ->
        Response.Status.proxy_request_refused
  in
  Response.response status body
