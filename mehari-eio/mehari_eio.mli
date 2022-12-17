module Direct : sig
  type 'a t = 'a

  val return : 'a -> 'a
end

include Mehari.NET with module IO = Direct

(** {1 Entry point} *)

val run :
  ?port:int ->
  ?backlog:int ->
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  (Uri.t -> Mehari.response) ->
  'a
