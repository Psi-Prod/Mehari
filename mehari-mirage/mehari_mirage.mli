(** Mehari and Mirage sitting on a tree. *)

open Mehari

val run :
  ?timeout:float ->
  ?port:int ->
  certs:Tls.Config.certchain list ->
  Mnet.TCP.state ->
  handler ->
  unit
(** [run ?port ?timeout ~certs tcp handler] runs a Gemini server.

    - [port] is the port to listen on. Defaults to [1965].
    - [timeout] is the maximum time in seconds to wait for client to write a
      request after the TLS handshake. Defaults to 5 seconds.
    - [certs] is the TLS certchains used. See section "Certificate selection" in
      {!Mehari.Server} module.

    @raise Invalid_argument msg on invalid TLS config. *)
