type t = {
  addr : Ipaddr.t * int;
  uri : Uri.t;
  sni : string option;
  params : (string * string) list;
}

let make ~uri ~addr ~sni = { uri; addr; sni; params = [] }
let attach_params t params = { t with params }
let set_uri t uri = { t with uri = Uri.of_string uri }
let uri { uri; _ } = uri
let ip { addr = ip, _; _ } = ip
let port { addr = _, port; _ } = port
let sni { sni; _ } = sni

let param t p =
  Printf.printf "%S\n" p;
  match List.assoc_opt p t.params with
  | None -> invalid_arg "Request.param"
  | Some p -> p
