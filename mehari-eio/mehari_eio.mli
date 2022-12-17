module Direct = Direct
include Mehari.NET with module IO := Direct

val run :
  ?port:int ->
  ?backlog:int ->
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  (Uri.t -> Mehari.response) ->
  'a
