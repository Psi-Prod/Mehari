(** Common signatures. *)

(** Describe a clock implementation. *)
module type PCLOCK = sig
  type t

  val now_d_ps : t -> int * int64
end

(** IO monad. *)
module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** Describe a logger implementation. *)
module type LOGGER = sig
  module IO : IO

  type clock
  type handler = Request.t -> Response.t IO.t

  val set_level : Logs.level -> unit
  val logger : clock -> handler -> handler
  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

(** Describe a rate limiter implementation. *)
module type RATE_LIMITER = sig
  module IO : IO

  type t
  type clock

  val check : t -> Request.t -> Response.t IO.t option

  val make :
    clock -> ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
end

(** Describe a generic router. *)
module type ROUTER = sig
  module IO : IO

  type route
  type rate_limiter
  type handler = Request.t -> Response.t IO.t
  type middleware = handler -> handler
  type domain_handler

  val route :
    ?rate_limit:rate_limiter ->
    ?middlewares:middleware list ->
    ('continuation, handler) Path.t ->
    'continuation ->
    route

  val router : route list -> handler
  val pipeline : middleware list -> middleware
  val domain : ?all:handler -> string -> handler -> domain_handler
  val virtual_host : domain_handler list -> handler
end

(** Describe a generic file system. *)
module type FILE_SYSTEM = sig
  module IO : IO

  type path

  val exists : path -> bool IO.t
  val kind : path -> [ `Regular_file | `Directory | `Other ] IO.t
  val read : path -> string list IO.t
  val concat : path -> string -> path
  val respond_document : ?mime:Mime.t -> path -> Response.t IO.t
  val pp_io_err : Format.formatter -> exn -> unit
end

(** Static file serving features. *)
module type STATIC = sig
  module IO : IO

  type handler = Request.t -> Response.t IO.t
  type dir_path

  val static :
    ?handler:(dir_path -> handler) ->
    ?dir_listing:
      (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    dir_path ->
    string ->
    handler
end

(** Module type containing all environment-dependent functions. *)
module type NET = sig
  module IO : IO

  type handler = Request.t -> Response.t IO.t
  (** Handlers are asynchronous functions from {!type:Mehari.request} to
      {!type:Mehari.response}. *)

  type route
  (** Routes tell {!val:router} which handler to select for each request. See
      {!section-routing}. *)

  type rate_limiter
  (** Rate limiter. See {!section-rate_limit}. *)

  type middleware = handler -> handler
  (** Middlewares take a {!type:handler}, and run some code before or after —
      producing a “bigger” {!type:handler}. See {!section-middleware}. *)

  type domain_handler
  (** Handler at domain name level. *)

  type clock
  (** System clock used by rate limiter. *)

  (** {1:middleware Middleware} *)

  val pipeline : middleware list -> middleware
  (** Combines a list of middlewares into one, such that these two lines are
      equivalent: [Mehari.pipeline [ mw1 ; mw2 ] @@ handler]
      [ mw1 @@ mw2 @@ handler]. *)

  (** {1:routing Routing} *)

  val route :
    ?rate_limit:rate_limiter ->
    ?middlewares:middleware list ->
    ('continuation, handler) Path.t ->
    'continuation ->
    route
  (** [route ~rate_limit ~middlewares path handler] forwards requests for [path]
      to [handler]. If rate limit is in effect, [handler] is not executed and a
      respond with {!type:Mehari.status} {!val:Mehari.Response.Status.slow_down}
      is sended. No middlewares and no rate limiter is attached by default. *)

  val router : route list -> handler
  (** Creates a router. If none of the routes match the {!type:Mehari.request},
      the router returns {!val:Mehari.Response.Status.not_found}. *)

  (** {1:rate_limit Rate limit} *)

  val make_rate_limit :
    clock ->
    ?period:int ->
    int ->
    [ `Second | `Minute | `Hour | `Day ] ->
    rate_limiter
  (** [make_rate_limit clock ~period n unit] creates a {!type:rate_limiter}
      which limits client to [n] request per [period * unit]. For example,
      {[
        make_rate_limit ~period:2 5 `Hour
      ]}
      limits client to 5 requests every 2 hours. *)

  (** {1:host Virtual hosting} *)

  val domain : ?all:handler -> string -> handler -> domain_handler
  (** [domain ?all name handler] attaches [handler] to the domain name [name].
      If [all] is set, it is used to handle domain names of form [*.name].

      @raise Invalid_argument if [name] is not a valid domain name. *)

  val virtual_host : domain_handler list -> handler
  (** [virtual_host domain_handlers] creates a {!type:handler} which allows
      virtual hosting using TLS Server Name Indication. Respond not found if the
      server Common Name is an IP address. *)

  (** {1 Logging} *)

  val set_log_lvl : Logs.level -> unit
  (** Set Mehari's logger to the given log level. *)

  val logger : clock -> middleware
  (** Logs and times requests. Time spent logging is included. *)

  val debug : 'a Logs.log
  val info : 'a Logs.log
  val warning : 'a Logs.log
  val error : 'a Logs.log
end

(** Module type containing all file system relative features. *)
module type FS = sig
  module IO : IO

  type handler = Request.t -> Response.t IO.t
  type path

  (** {1 Static files} *)

  val respond_document : ?mime:Mime.t -> path -> Response.t IO.t
  (** Same as {!val:Mehari.Response.respond} but respond with content of given
      [filename] and use given {!type:Mehari.mime} as mime type. If [filename]
      is not present on filesystem or an error occured, responds with
      {!val:Mehari.Response.Status.not_found}. If [mime] parameter is not
      supplied, document is served as {!val:Mehari.Mime.app_octet_stream}. *)

  val static :
    ?handler:(path -> handler) ->
    ?dir_listing:
      (([ `Regular_file | `Directory | `Other ] * string) list -> handler) ->
    ?index:string ->
    ?show_hidden:bool ->
    path ->
    string ->
    handler
  (** [static dir target] validates [target] by checking that it is relative and
      does not contain parent directory references. If these checks fail,
      responds with {!val:Mehari.Response.Status.not_found}.

      If the checks succeed, [static] calls [handler path request], where [path]
      is the path generated by the concatenation of directory that was passed to
      [static] and path of request. [handler] defaults to
      {!val:respond_document}.

      If a directory is requested, [static] will look for a file named [index]
      in that directory to return. Otherwise, a directory file listing will be
      generated by calling [dir_listing [ filename; ... ] request]. [index] is
      default on [index.gmi].

      Mime type of the served ressource is guessed by checking file name. Note
      that file names of the form [*.gmi] will be served as [text/gemini].

      [show_hidden] decides whether hidden files should be listed. It defaults
      to [false] for security reasons. *)
end

(** Module type containing all CGI scripting relative features. *)
module type CGI = sig
  (** Mehari supports CGI scripting as described in RFC 3875

      The CGI script must write its gemini response on its stdout. Status code
      and meta string are on the first line, and the optional response body on
      subsequent lines. The bytes generated by the CGI script will be forwarded
      verbatim to the Gemini client, without any additional modification by the
      server.

      Some variables ([CONTENT_LENGTH], [CONTENT_TYPE], [REMOTE_IDENT],
      [REMOTE_METHOD], [REMOTE_USER], [REQUEST_METHOD]) volontary have an empty
      string value to be compatibile as much as possible with non-gemini CGI
      script.

      @see < https://www.rfc-editor.org/rfc/rfc3875 > For the CGI specification.

      {2 Examples}

      Let's assume that client requested the following URL:
      [gemini://localhost/cgi/script.cgi?an_input] and the response generation
      is handled by a CGI script located at [/var/cgi-bin/script.cgi] on server.
      CGI variables values are:

      - [AUTH_TYPE] is ["Certificate"] if a client certificate is provided,
        empty otherwise.
      - [CONTENT_LENGTH] is empty
      - [CONTENT_TYPE] is empty
      - [GATEWAY_INTERFACE] is [CGI/1.1]
      - [PATH_INFO] is [/cgi/script.cgi]
      - [PATH_TRANSLATED] is [/cgi/script.cgi]
      - [QUERY_STRING] is [an_input]
      - [REMOTE_ADDR] is [127.0.0.1:client_port]
      - [REMOTE_HOST] is same as [127.0.0.1]
      - [REMOTE_IDENT] is empty
      - [REMOTE_METHOD] is empty
      - [REMOTE_USER] is empty
      - [REQUEST_METHOD] is empty
      - [SCRIPT_NAME] is [/var/cgi-bin/script.cgi]
      - [SERVER_NAME] is [localhost]
      - [SERVER_PORT] is [1965]
      - [SERVER_PROTOCOL] is [GEMINI]
      - [SERVER_SOFTWARE] is [Mehari/1.0]
      - [TLS_CLIENT_HASH] is the SHA-256 sum of the client certificate if it is
        provided, empty otherwise
      - [TLS_CLIENT_SUBJECT] is the client certificate Subject common name if it
        is provided, empty otherwise
      - [TLS_CLIENT_ISSUER] is the client certificate Issuer common name if it
        is provided, empty otherwise *)

  module IO : IO

  val run_cgi :
    ?timeout:float -> ?non_parsed:bool -> string -> Request.t -> Response.t IO.t
  (** [run_cgi ?timeout ?non_parsed script_path req] executes the CGI script
      located at [script_path] and return a {!type:Mehari.response} containing
      script's stdout output. Responds with {!val:Mehari.Status.cgi_error} in
      case of error or [timeout] exceeding.

      - [timeout] is expressed in seconds and defaults to [5.0].
      - [non_parsed] decides if NPH (Non-Parsed Header CGI feature) is enable.
        It allow to passes all responsibility for response processing to script
        which means that server no longer checks correctness of the script
        output. Use it with caution. Defaults to [false]. *)
end

(* Common server functionnality to define in each implementation. *)
module type SERVER = sig
  module IO : IO

  type config
  (** System dependant configuration. *)

  val run :
    ?port:int ->
    ?timeout:float ->
    ?config:config ->
    certs:Certs.t ->
    (Request.t -> Response.t IO.t) ->
    unit IO.t
  (** [run ?port ?verify_url_host ?timeout ?config ~certs handler] runs a Gemini
      server.

      - [port] is the port to listen on. Defaults to [1965].
      - [timeout] is the maximum time to wait for client to write a request
        after the TLS handshake. Unset by default.
      - [certs] is the TLS certchains used. See section "Certificate selection"
        in {!Mehari.Certs}.
      - [config] is the implementation dependant configuration used. *)
end
