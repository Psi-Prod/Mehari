type t =
  | Single of certchain
  | Multiple of certchain list
  | Multiple_default of certchain * certchain list

and certchain = Tls.Config.certchain

module Private = struct
  let to_own_cert = function
    | Single c -> `Single c
    | Multiple cs -> `Multiple cs
    | Multiple_default (c, cs) -> `Multiple_default (c, cs)

  let make_config certs =
    let certificates = to_own_cert certs in
    Tls.Config.server ~version:(`TLS_1_2, `TLS_1_3) ~certificates
      ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
      ()
    |> Result.get_ok
end
