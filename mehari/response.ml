type t = { status : int option; kind : kind }

and kind =
  | Immediate of string list
  (* A list for avoid ( ^ ) quadatric concatenation. *)
  | Delayed of stream

and stream = { body : (string -> unit) -> unit; flush : bool }

type view = kind

type 'a status = int * 'a typ

and _ typ =
  | Success : body -> Mime.t typ
  | Meta : string typ

and body = String of string | Gemtext of Gemtext.t | Stream of stream

let view_of_resp r = r.kind
let string t = String t
let gemtext g = Gemtext g
let stream ?(flush = false) body = Stream { body; flush }

let lines l =
  stream ~flush:false (fun consume ->
      List.iter
        (fun line ->
          consume line;
          consume "\n")
        l)

let seq ?flush s = stream ?flush (fun consume -> Seq.iter consume s)

let page ~title body =
  gemtext Gemtext.[ heading `H1 title; text "\n"; text body ]

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
    | None -> Immediate [ meta ]
    | Some (String t) -> Immediate [ meta; t ]
    | Some (Gemtext g) -> Immediate [ meta; Gemtext.to_string g ]
    | Some (Stream { body; flush }) ->
        Delayed
          {
            body =
              (fun consume ->
                consume meta;
                body consume);
            flush;
          }

let to_response (type a) ((code, status) : a status) (m : a) =
  let meta, body =
    match status with
    | Success body -> (Mime.to_string m, Some body)
    | Meta -> (m, None)
  in
  { status = Some code; kind = validate code meta body }

module Status = struct
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

let response status info = to_response status info
let response_body body = response (Status.success body)
let response_text txt = response (Status.success (string txt)) Mime.plaintext

let response_gemtext ?charset ?lang g =
  Mime.gemini ?charset ?lang () |> response (Status.success (gemtext g))

let response_raw raw =
  match raw with
  | `Body b -> { status = None; kind = Immediate [ b ] }
  | `Full (code, meta, body) ->
      { status = Some code; kind = Immediate [ fmt_meta code meta; body ] }
