type 'a t = {
  addr : 'a;
  addrm : (module Types.ADDR with type t = 'a);
  port : int;
  uri : Uri.t;
  sni : string option;
  params : Re.Group.t option;
}

let uri { uri; _ } = uri
let ip { addr; _ } = addr
let port { port; _ } = port
let sni { sni; _ } = sni
let query { uri; _ } = Uri.verbatim_query uri

let make (type a) (module Addr : Types.ADDR with type t = a) ~uri ~(addr : a)
    ~port ~sni =
  { uri; addr; addrm = (module Addr); port; sni; params = None }

let attach_params t params = { t with params }

let param t p =
  let fail () = invalid_arg "Mehari.param" in
  match t.params with
  | None -> fail ()
  | Some _ when p <= 0 -> fail ()
  | Some grp -> (
      match Re.Group.get_opt grp p with None -> fail () | Some param -> param)
