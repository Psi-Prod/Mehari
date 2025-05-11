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

module type NET = sig
  module IO : Signatures.IO

  type route
  type rate_limiter
  type handler = request -> response IO.t
  type middleware = handler -> handler
  type clock

  val no_middleware : middleware
  val pipeline : middleware list -> middleware
  val router : route list -> handler

  val route :
    ?rate_limit:rate_limiter ->
    ?mw:middleware ->
    ?regex:bool ->
    string ->
    handler ->
    route

  val scope :
    ?rate_limit:rate_limiter -> ?mw:middleware -> string -> route list -> route

  val no_route : route

  val make_rate_limit :
    clock ->
    ?period:int ->
    int ->
    [ `Second | `Minute | `Hour | `Day ] ->
    rate_limiter

  val virtual_hosts :
    ?meth:[ `ByURL | `SNI ] -> (string * handler) list -> handler

  val set_log_lvl : Logs.level -> unit
  val logger : clock -> handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

module type FS = sig
  module IO : Signatures.IO

  type handler = request -> response IO.t
  type dir_path

  val respond_document : ?mime:mime -> dir_path -> response IO.t

  val static :
    ?handler:(dir_path -> handler) ->
    ?dir_listing:
      (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    dir_path ->
    handler
end

module Private = struct
  module type IO = Signatures.IO
  module type PCLOCK = Signatures.PCLOCK

  module Cert = struct
    let get_certs ~exn_msg = function
      | default :: mult -> `Multiple_default (default, mult)
      | _ -> invalid_arg exn_msg
  end

  module Cgi = Cgi
  module Logger_impl = Logger_impl
  module Protocol = Protocol
  module Rate_limiter_impl = Rate_limiter_impl
  module Router_impl = Router_impl
  module Static = Static
end
