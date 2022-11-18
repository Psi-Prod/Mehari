module StringMap = Map.Make (String)

type t = {
  addr : Ipaddr.t * int;
  uri : Uri.t;
  sni : string option;
  params : string StringMap.t;
}

let make ~uri ~addr ~sni = { uri; addr; sni; params = StringMap.empty }
let uri { uri; _ } = uri
let ip { addr = ip, _; _ } = ip
let port { addr = _, port; _ } = port
let sni { sni; _ } = sni
