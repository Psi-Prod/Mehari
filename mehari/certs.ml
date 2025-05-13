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

  let supports_hostname certs host =
    let supports_hostname =
      List.exists (fun c -> X509.Certificate.supports_hostname c host)
    in
    match certs with
    | Single (certs, _) -> supports_hostname certs
    | Multiple [] -> false
    | Multiple certs -> List.exists (fun (cs, _) -> supports_hostname cs) certs
    | Multiple_default ((certs, _), certss) ->
        supports_hostname certs
        || List.exists (fun (cs, _) -> supports_hostname cs) certss
end
