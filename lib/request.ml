type t = {
  addr : Lwt_unix.sockaddr;
  uri : Uri.t;
  sni : string option;
  params : (string * string) list;
}

let make ~uri ~addr ~sni = { uri; addr; sni; params = [] }
let attach_params t params = { t with params }
let set_uri t uri = { t with uri = Uri.of_string uri }
let uri { uri; _ } = uri

let addr { addr; _ } =
  match addr with Unix.ADDR_INET (addr, _) -> addr | _ -> assert false

let port { addr; _ } =
  match addr with Unix.ADDR_INET (_, port) -> port | _ -> assert false

let sni { sni; _ } = sni

let param t p =
  Printf.printf "%S\n" p;
  match List.assoc_opt p t.params with
  | None -> invalid_arg "Request.param"
  | Some p -> p
