module StringMap = Map.Make (String)

type t = { addr : Lwt_unix.sockaddr; uri : Uri.t; params : string StringMap.t }

let make ~uri ~addr = { uri; addr; params = StringMap.empty }
let uri { uri; _ } = uri

let addr { addr; _ } =
  match addr with Unix.ADDR_INET (addr, _) -> addr | _ -> failwith ""

let port { addr; _ } =
  match addr with Unix.ADDR_INET (_, port) -> port | _ -> failwith ""
