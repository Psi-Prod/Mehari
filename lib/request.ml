module StringMap = Map.Make (String)

type t = {
  addr : Lwt_unix.sockaddr;
  uri : Uri.t;
  sni : string option;
  params : string StringMap.t;
}

let make ~uri ~addr ~sni = { uri; addr; sni; params = StringMap.empty }
let uri { uri; _ } = uri

let addr { addr; _ } =
  match addr with Unix.ADDR_INET (addr, _) -> addr | _ -> assert false

let port { addr; _ } =
  match addr with Unix.ADDR_INET (_, port) -> port | _ -> assert false

let sni { sni; _ } = sni
