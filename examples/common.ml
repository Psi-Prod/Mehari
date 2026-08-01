let load_certs ~cert ~priv_key =
  let certs =
    let pem = In_channel.(with_open_text cert input_all) in
    match X509.Certificate.decode_pem_multiple pem with
    | Ok cs -> cs
    | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m)
  and priv_key =
    let pem = In_channel.(with_open_text priv_key input_all) in
    match X509.Private_key.decode_pem pem with
    | Ok key -> key
    | Error (`Msg m) -> invalid_arg ("failed to parse private key " ^ m)
  in
  [ (certs, priv_key) ]
