(** An IO module Mehari implementation based on [Eio] library. *)

module Addr = Common.Addr
module Direct = Common.Direct

(** {1 Net} *)

(** @closed *)
include Mehari.NET with module IO := Direct and type addr = Addr.t

(** @closed *)
include
  Mehari.FS
    with module IO := Direct
     and type addr = Addr.t
     and type dir_path := Eio.Fs.dir Eio.Path.t

(** {1 Entry point} *)

val run :
  ?port:int ->
  ?verify_url_host:bool ->
  ?config:Tls.Config.server ->
  ?timeout:float * Eio.Time.clock ->
  ?backlog:int ->
  ?addr:addr ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  handler ->
  unit
(** [run ?port ?verify_url_host ?config ?backlog ?addr certchains net handler] runs the server
    using [handler].

      - [port] is the port to listen on. Defaults to [1965].
      - [verify_url_host], if true (by default), will verify if the URL
        hostname corresponds to the server's certificate (chosen according to
        {{: https://github.com/mirleft/ocaml-tls/blob/main/sni.md }ocaml-tls sni.md}).
      - [config] is the TLS server configuration.
        Defaults to
        {@ocaml[
          Tls.Config.server ~certificates
              ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
              ()
        ]}
        To support client certificates, specify the [authenticator].
      - [timeout] is a couple of form [(duration, eio_clock)]. [duration] is
        the maximum waiting time in seconds for the client to write a request
        after TLS handshake. Unset by default.
      - [backlog] is the the number of pending connections that can be queued
        up. Defaults to [4096].
      - [addr] is the socket addresses. Defaults to
        [Eio.Net.Ipaddr.V4.loopback].
      - [certchains] is the list of form [[(cert_path, private_key_path); ...]],
        the last one is considered default.

    @raise Invalid_argument if [certchains] is empty. *)
