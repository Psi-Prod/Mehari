open Mirage

let static_key = Key.(value @@ kv_ro ~group:"static" ())
let static = generic_kv_ro ~key:static_key "gemtext"
let certs_key = Key.(value @@ kv_ro ~group:"certs" ())
let certs = generic_kv_ro ~key:certs_key "tls"

let main =
  main
    ~packages:
      [
        package ~pin:"git+https://github.com/Psi-Prod/Mehari#dev" "mehari";
        package ~pin:"git+https://github.com/Psi-Prod/Mehari#dev"
          "mehari-mirage";
      ]
    "Unikernel.GeminiServer"
    (kv_ro @-> kv_ro @-> stackv4v6 @-> job)

let () =
  register "gemini-srv"
    [ main $ static $ certs $ generic_stackv4v6 default_network ]
