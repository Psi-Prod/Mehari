(** Mirage OS compatiblity. *)

module type S = sig
  module IO = Lwt

  type stack
  (** Tcpip stack. *)

  (** {1 Net} *)

  (** @closed *)
  include Mehari.NET with module IO := IO and type addr = Ipaddr.t

  (** {1 Response} *)

  val respond : 'a Mehari.status -> 'a -> Mehari.response IO.t
  (** Same as {!val:Mehari.response}, but the new {!type:Mehari.response} is
        wrapped in a promise. *)

  val respond_body : Mehari.body -> Mehari.mime -> Mehari.response IO.t
  (** Same as {!val:respond} but respond with given {!type:Mehari.body}
        and use given {!type:Mehari.mime} as mime type. *)

  val respond_text : string -> Mehari.response IO.t
  (** Same as {!val:respond} but respond with given text and use [text/plain] as
        {!type:Mehari.mime} type. *)

  val respond_gemtext :
    ?charset:string ->
    ?lang:string list ->
    Mehari.Gemtext.t ->
    Mehari.response IO.t
  (** Same as {!val:respond} but respond with given {!type:Mehari.Gemtext.t} and use
        [text/gemini] as {!type:Mehari.mime} type. *)

  val respond_raw :
    [ `Body of string | `Full of int * string * string ] -> Mehari.response IO.t
  (** Same as {!val:Mehari.response_raw}, but the new {!type:Mehari.response}
        is wrapped in a promise. *)

  (** {1 Entry point} *)

  val run :
    ?port:int ->
    ?timeout:float ->
    ?verify_url_host:bool ->
    ?config:Tls.Config.server ->
    ?certchains:(string * string) list ->
    stack ->
    handler ->
    unit IO.t
  (** [run ?port ?timeout ?verify_url_host ?config ?certchains stack handler]
      runs the server using [host].

        - [port] is the port to listen on. Defaults to [1965].
        - [timeout] is the maximum waiting time in seconds for the client to
          write a request after TLS handshake. Unset by default.
        - [verify_url_host], if true (by default), will verify if the URL hostname
          corresponds to the server's certificate
          (chosen according to
          {{: https://github.com/mirleft/ocaml-tls/blob/main/sni.md }ocaml-tls sni.md}).
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
end

(** A functor building an IO module from Mirage components. *)
module Make
    (Clock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S) : S with type stack = Stack.t
