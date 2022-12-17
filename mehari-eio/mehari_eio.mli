module Direct = Common.Direct

include
  Mehari.NET with module IO := Common.Direct and type addr = Eio.Net.Ipaddr.v4v6

val run :
  ?port:int ->
  ?backlog:int ->
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  certchains:(Eio.Fs.dir Eio.Path.t * Eio.Fs.dir Eio.Path.t) list ->
  Eio.Net.t ->
  handler ->
  'a
