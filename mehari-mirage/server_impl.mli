(** Server implementation. *)

module Make : (Stack : Tcpip.Stack.V4V6)
  (Time : Mirage_time.S)
  (Logger : Mehari.Private.Signatures.LOGGER)
  -> sig
  type stack = Stack.t

  type handler = Mehari.request -> Mehari.response Lwt.t

  val run :
    ?port:int ->
    ?verify_url_host:bool ->
    ?timeout:float ->
    certs:Mehari.Certs.t ->
    stack ->
    handler ->
    unit Lwt.t
end
