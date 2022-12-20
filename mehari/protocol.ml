type request_err =
  | AboveMaxSize
  | EmptyURL
  | InvalidURL
  | MalformedUTF8
  | MissingHost
  | MissingScheme
  | RelativePath
  | WrongHost
  | WrongPort
  | WrongScheme

(* Perform some static check on given request *)
let static_check_request ~port ~hostnames input =
  try
    let utf8 = Zed_string.of_utf8 input |> Zed_string.to_utf8 in
    let length = Bytes.of_string utf8 |> Bytes.length in
    if length = 0 then Error EmptyURL
    else if length > 1024 then Error AboveMaxSize
    else
      let uri = Uri.of_string utf8 in
      match Uri.scheme uri with
      | None -> Error MissingScheme
      | Some scheme when scheme <> "gemini" -> Error WrongScheme
      | Some _ -> (
          if Uri.path uri |> Filename.is_relative then Error RelativePath
          else
            match Uri.host uri with
            | None -> Error MissingHost
            | Some h -> (
                match Seq.find (String.equal h) hostnames with
                | None -> Error WrongHost
                | Some _ -> (
                    match Uri.port uri with
                    | None -> Ok uri
                    | Some p when Int.equal port p -> Ok uri
                    | Some _ -> Error WrongPort)))
  with Zed_string.Invalid _ | Zed_utf8.Invalid _ -> Error MalformedUTF8

let pp_err fmt =
  let fmt = Format.fprintf fmt in
  function
  | AboveMaxSize -> fmt "Request has a size higher than 1024 bytes"
  | EmptyURL -> fmt "URL is empty"
  | InvalidURL -> fmt "invalid URL"
  | MalformedUTF8 -> fmt "URL contains non-UTF8 byte sequence"
  | MissingScheme -> fmt "URL has no scheme"
  | MissingHost -> fmt "The host URL subcomponent is required"
  | RelativePath -> fmt "URL path is relative"
  | WrongHost -> fmt "URL contains a foreign hostname"
  | WrongPort -> fmt "URL has an incorrect port number"
  | WrongScheme -> fmt {|URL scheme is not "gemini://"|}

let to_response err =
  let body = Format.asprintf "%a" pp_err err in
  let status =
    match err with
    | AboveMaxSize | EmptyURL | InvalidURL | MalformedUTF8 | MissingHost
    | MissingScheme | RelativePath ->
        Response.Status.bad_request
    | WrongHost | WrongPort | WrongScheme ->
        Response.Status.proxy_request_refused
  in
  Response.response status body
