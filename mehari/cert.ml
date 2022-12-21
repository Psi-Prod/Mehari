module type X509 = sig
  module IO : Types.IO

  type path

  val private_of_pems : cert:path -> priv_key:path -> Tls.Config.certchain IO.t
end

module type S = sig
  module IO : Types.IO

  type path

  val get_certs :
    exn_msg:string -> (path * path) list -> Tls.Config.own_cert IO.t
end

module Make (X : X509) : S with module IO := X.IO and type path := X.path =
struct
  let ( let* ) = X.IO.bind

  let load_certs certs =
    let rec aux acc = function
      | [] -> X.IO.return acc
      | (cert, priv_key) :: tl ->
          let* certchain = X.private_of_pems ~cert ~priv_key in
          aux (certchain :: acc) tl
    in
    aux [] certs

  let get_certs ~exn_msg certchains =
    let* certs = load_certs certchains in
    match certs with
    | c :: _ -> `Multiple_default (c, certs) |> X.IO.return
    | _ -> invalid_arg exn_msg
end
