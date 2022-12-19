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
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  handler ->
  'a
