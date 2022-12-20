(** An IO module Mehari implementation based on [Eio] library. *)

(** {1 Net} *)

module Direct = Common.Direct

(** @closed *)
include Mehari.NET with module IO := Direct and type addr = Eio.Net.Ipaddr.v4v6

(** {1 Response} *)

val response_document : Mehari.mime -> Eio.Fs.dir Eio.Path.t -> Mehari.response
(** Same as {!val:Mehari.response} but respond with content of file located at
    path and use given {!type:Mehari.mime} as mime type. The whole file content
    is not loaded in memory: response is chunked. *)

(** {1 Entry point} *)

val run :
  ?port:int ->
  ?backlog:int ->
  ?addr:addr ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  handler ->
  unit
(** [run ?port ?backlog ?addr ~certchains net handler] runs the server using
    [handler].
      - [port] is the port to listen on. Defaults to [1965].
      - [backlog] is the the number of pending connections that can be queued
        up. Defaults to [10].
      - [addr] is the socket addresses. Defaults to
        [Eio.Net.Ipaddr.V4.loopback].
      - [certchains] is the list of form [[(cert_path, private_key_path); ...]],
        the last one is considered default.

      @raise Invalid_argument if [certchains] is empty. *)
