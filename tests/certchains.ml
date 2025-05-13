let private_of_pems ~cert ~priv_key =
  let certs =
    try
      let pem = In_channel.(with_open_text cert input_all) in
      match X509.Certificate.decode_pem_multiple pem with
      | Ok cs -> cs
      | Error (`Msg m) -> invalid_arg ("failed to parse certificates " ^ m)
    with Invalid_argument m ->
      Fmt.failwith "Private certificates %S: %s" cert m
  in
  let pk =
    try
      let pem = In_channel.(with_open_text priv_key input_all) in
      match X509.Private_key.decode_pem pem with
      | Ok key -> key
      | Error (`Msg m) -> invalid_arg ("failed to parse private key " ^ m)
    with Invalid_argument m -> Fmt.failwith "Private key (%S): %s" priv_key m
  in
  (certs, pk)

(* Generated with openssl req -x509 -newkey rsa:4096 -keyout heyplzlookatme_key.pem -out heyplzlookatme_cert.pem -sha256 -days 365 -nodes --subj "/CN=heyplzlookat.me" *)
let heyplzlookatme =
  private_of_pems ~cert:"./certchains/heyplzlookatme_cert.pem"
    ~priv_key:"./certchains/heyplzlookatme_key.pem"

(* Generated with openssl req -x509 -newkey rsa:4096 -keyout localhost_key.pem -out localhost_cert.pem -sha256 -days 365 -nodes --subj "/CN=localhost" *)
let localhost =
  private_of_pems ~cert:"./certchains/localhost_cert.pem"
    ~priv_key:"./certchains/localhost_key.pem"

(* Generated with openssl req -x509 -newkey rsa:4096 -keyout geminiprotocolnet_key.pem -out geminiprotocolnet_cert.pem -sha256 -days 365 -nodes --subj "/CN=geminiprotocol.net" *)
let geminiprotocolnet =
  private_of_pems ~cert:"./certchains/geminiprotocolnet_cert.pem"
    ~priv_key:"./certchains/geminiprotocolnet_key.pem"
