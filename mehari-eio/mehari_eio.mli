(** An IO module Mehari implementation based on [Eio] library. *)

(** {1 Net} *)

module Direct = Common.Direct

(** @closed *)
include Mehari.NET with module IO := Direct and type addr = Eio.Net.Ipaddr.v4v6

(** {1 Static files} *)

val response_document :
  ?mime:Mehari.mime -> Eio.Fs.dir Eio.Path.t -> Mehari.response
(** Same as {!val:Mehari.response} but respond with content of file located at
    path. If [mime] parameter is not supplied, use {!val:Mehari.no_mime} as
    mime type. The whole file content is not loaded in memory: response is
    chunked. *)

val static :
  ?handler:(Eio.Fs.dir Eio.Path.t -> string -> handler) ->
  ?listing:(string -> string list -> Mehari.response) ->
  ?show_hidden:bool ->
  Eio.Fs.dir Eio.Path.t ->
  handler
(** [static ?show_hidden dir] validates the path parameter (retrieves by
    calling [Mehari.param req 1]) by cheking that it is relative and does not
    contain parent directory references. If these checks fail,
    responds with {!val:Mehari.not_found}.

    If the checks succeed, [static] calls [handler path path_str request],
    where [path] and [path_str] are respectively the path generated by the
    concatenation of directory that was passed to [static] and path of
    request as an {!type:Eio.Path.t} and a string. [handler] defaults to
    {!val:response_document}.

    A directory listing is generated if the path parameter of the query is a
    directory by calling [listing path_param [ filename; ... ]].

    [show_hidden] decides whether hidden files should be listed. It defaults to
    false for security reasons and has no effect if listing is supplied. *)

(** {1 Entry point} *)

val run :
  ?port:int ->
  ?backlog:int ->
  ?timeout:float * Eio.Time.clock ->
  ?addr:addr ->
  ?config:Tls.Config.server ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  handler ->
  unit
(** [run ?port ?backlog ?addr ?config ~certchains net handler] runs the server using
    [handler].
      - [port] is the port to listen on. Defaults to [1965].
      - [backlog] is the the number of pending connections that can be queued
        up. Defaults to [4096].
      - [timeout] is a couple of form [(duration, eio_clock)]. [duration] is
        the maximum waiting time in seconds for the client to write a request
        after TLS handshake. Unset by default.
      - [addr] is the socket addresses. Defaults to
        [Eio.Net.Ipaddr.V4.loopback].
      - [config] is the TLS server configuration.
        Defaults to
        {@ocaml[
          Tls.Config.server ~certificates
              ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
              ()
        ]}
        To support client certificates, specify the [authenticator].
      - [certchains] is the list of form [[(cert_path, private_key_path); ...]],
        the last one is considered default.

      @raise Invalid_argument if [certchains] is empty. *)
