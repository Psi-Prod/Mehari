(** An IO module Mehari implementation based on [Eio] library. *)

open Mehari

(** {1 Net} *)

module IO = Identity_reader_monad

(** @closed *)
include
  NET with module IO := IO and type clock := [ `Clock of float ] Eio.Time.clock

(** @closed *)
include FS with module IO := IO and type dir_path := [ `Dir ] Eio.Path.t

(** {1 Entry point} *)

val run :
  ?port:int ->
  ?verify_url_host:bool ->
  ?timeout:float ->
  ?backlog:int ->
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  certs:Certs.t ->
  handler ->
  unit IO.t
(** [run ?port ?verify_url_host ?backlog ?addr ~certchains handler] runs the
    server using [handler].

    - [port] is the port to listen on. Defaults to [1965].
    - [verify_url_host], if true (by default), will verify if the URL hostname
      corresponds to the server's certificate (chosen according to
      {{:https://github.com/mirleft/ocaml-tls/blob/main/sni.md}ocaml-tls sni.md}).
    - [timeout] is the maximum waiting time in seconds for the client to write a
      request after TLS handshake. Unset by default.
    - [backlog] is the the number of pending connections that can be queued up.
      Defaults to [4096].
    - [addr] is the socket addresses. Defaults to [Eio.Net.Ipaddr.V4.loopback].
    - [certs] is the TLS certchains used. *)
