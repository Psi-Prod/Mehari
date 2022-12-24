type 'a t = {
  addr : 'a;
  addrm : (module Types.ADDR with type t = 'a);
  port : int;
  uri : Uri.t;
  sni : string;
  params : Re.Group.t option;
  client_cert : X509.Certificate.t list;
}

let uri { uri; _ } = uri
let ip { addr; _ } = addr
let port { port; _ } = port
let sni { sni; _ } = sni
let query { uri; _ } = Uri.verbatim_query uri
let client_cert { client_cert; _ } = client_cert

let make (type a) (module Addr : Types.ADDR with type t = a) ~uri ~(addr : a)
    ~port ~sni ~client_cert =
  { uri; addr; addrm = (module Addr); port; sni; params = None; client_cert }

let attach_params t params = { t with params }

let param t p =
  let fail () = invalid_arg "Mehari.param" in
  match t.params with
  | None -> fail ()
  | Some _ when p <= 0 -> fail ()
  | Some grp -> (
      match Re.Group.get_opt grp p with None -> fail () | Some param -> param)
