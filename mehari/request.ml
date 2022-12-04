type t = {
  addr : Ipaddr.t * int;
  uri : Uri.t;
  sni : string option;
  params : Re.Group.t option;
}

let make ~uri ~addr ~sni = { uri; addr; sni; params = None }
let attach_params t params = { t with params }
let set_uri t uri = { t with uri = Uri.of_string uri }
let uri { uri; _ } = uri
let ip { addr = ip, _; _ } = ip
let port { addr = _, port; _ } = port
let sni { sni; _ } = sni
let query { uri; _ } = Uri.verbatim_query uri

let param t p =
  let fail () = invalid_arg "Mehari.param" in
  match t.params with
  | None -> fail ()
  | Some _ when p <= 0 -> fail ()
  | Some grp -> (
      match Re.Group.get_opt grp p with None -> fail () | Some param -> param)
