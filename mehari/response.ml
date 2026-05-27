type t = Structured of response | Raw of string
and response = { status : int; body : body }
and body = String of string | Stream of stream
and stream = string Flux.stream

module Body = struct
  type t = String of string | Gemtext of Gemtext.t | Stream of stream

  let string t = String t
  let gemtext g = Gemtext g
  let stream s = Stream s

  let lines l =
    let open Flux in
    Stream.via
      (Flow.map (fun line -> line ^ "\n"))
      (Stream.from (Source.list l))
    |> stream

  let page ~title body =
    gemtext Gemtext.[ heading `H1 title; text "\n"; text body ]

  let seq s = stream Flux.(Stream.from (Source.seq s))
end

module Status = struct
  type 'a t = int * 'a typ
  and _ typ = Success : Body.t -> Mime.t typ | Meta : string typ

  let input = (10, Meta)
  let sensitive_input = (11, Meta)
  let success body = (20, Success body)
  let redirect_temp = (30, Meta)
  let redirect_perm = (31, Meta)
  let temporary_failure = (40, Meta)
  let server_unavailable = (41, Meta)
  let cgi_error = (42, Meta)
  let proxy_error = (43, Meta)
  let slow_down = (44, Meta)
  let perm_failure = (50, Meta)
  let not_found = (51, Meta)
  let gone = (52, Meta)
  let proxy_request_refused = (53, Meta)
  let bad_request = (59, Meta)
  let client_cert_req = (60, Meta)
  let cert_not_authorised = (61, Meta)
  let cert_not_valid = (62, Meta)
  let code_of_status (c, _) = c
end

let fmt_meta = Printf.sprintf "%i %s\r\n"

let is_startswith_bom = function
  | "" -> false
  | s ->
      String.get_utf_8_uchar s 0 |> Uchar.utf_decode_uchar
      |> Fun.flip List.mem
           [ Uchar.of_int 0xEF; Uchar.of_int 0xBB; Uchar.of_int 0xBF ]

let validate code meta body =
  if is_startswith_bom meta then
    invalid_arg "meta begins with a U+FEFF byte order mark"
  else if Bytes.(of_string meta |> length) > 1024 then
    invalid_arg "too long header"
  else
    let meta = fmt_meta code meta in
    match body with
    | None -> String meta
    | Some (Body.String t) -> String (meta ^ t)
    | Some (Gemtext g) -> String (meta ^ Gemtext.to_string g)
    | Some (Body.Stream s) ->
        let header = Flux.(Stream.from (Source.list [ meta ])) in
        Stream (Flux.Stream.concat header s)

let to_response (type a) ((code, status) : a Status.t) (m : a) =
  let meta, body =
    match status with
    | Success body -> (Mime.to_string m, Some body)
    | Meta -> (m, None)
  in
  Structured { status = code; body = validate code meta body }

let respond status info = to_response status info
let body body = respond (Status.success body)
let text txt = respond (Status.success (Body.string txt)) Mime.plaintext

let gemtext ?charset ?lang g =
  Mime.gemini ?charset ?lang () |> respond (Status.success (Body.gemtext g))

let status = function
  | Structured { status; _ } -> status
  | Raw _ -> invalid_arg "Mehari.Response.status"

module Private = struct
  type view = body = String of string | Stream of stream

  let to_view = function
    | Raw resp -> String resp
    | Structured { body; _ } -> body

  let raw status meta body =
    let body = String (fmt_meta status meta ^ body) in
    Structured { status; body }

  let unsafe_raw resp = Raw resp
end
