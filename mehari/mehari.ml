type request = Request.t
type response = Response.t
type 'a status = 'a Response.Status.t
type mime = Mime.t
type body = Response.Body.t

module Gemtext = Gemtext

let paragraph gemtext s =
  let doc = ref [] in
  let cr = ref false in
  let buf = Buffer.create (String.length s) in
  for i = 0 to String.length s - 1 do
    match String.unsafe_get s i with
    | '\r' -> cr := true
    | '\n' when !cr ->
        let line = Buffer.contents buf in
        Buffer.reset buf;
        doc := gemtext line :: !doc;
        cr := false
    | '\n' ->
        let line = Buffer.contents buf in
        Buffer.reset buf;
        doc := gemtext line :: !doc;
        cr := false
    | c ->
        if !cr then Buffer.add_char buf '\r';
        Buffer.add_char buf c;
        cr := false
  done;
  List.rev
  @@ match Buffer.contents buf with "" -> !doc | line -> gemtext line :: !doc

module Request = Request
module Response = Response
module Body = Response.Body
module Status = Response.Status
module Mime = Mime
module Certs = Certs

module type NET = Signatures.NET
module type FS = Signatures.FS
module type SERVER = Signatures.SERVER

module Private = struct
  module type IO = Signatures.IO
  module type PCLOCK = Signatures.PCLOCK

  module Cgi = Cgi
  module Logger_impl = Logger_impl
  module Protocol = Protocol
  module Rate_limiter_impl = Rate_limiter_impl
  module Router_impl = Router_impl
  module Signatures = Signatures
  module Static = Static
end
