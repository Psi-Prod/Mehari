open Mirage

let port =
  let doc =
    Key.Arg.info
      ~doc:"The TCP port on which to listen for incoming connections."
      [ "port" ]
  in
  Key.(create "port" Arg.(opt int 1965 doc))

let static_key = Key.(value @@ kv_ro ~group:"static" ())
let static = generic_kv_ro ~key:static_key "gemtext"
let certs_key = Key.(value @@ kv_ro ~group:"certs" ())
let certs = generic_kv_ro ~key:certs_key "tls"

let main =
  foreign
    ~keys:Key.[ v port ]
    ~packages:[ package "mehari"; package "mehari-mirage" ]
    "Unikernel.GeminiServer"
    (random @-> kv_ro @-> kv_ro @-> pclock @-> stackv4v6 @-> time @-> job)

let () =
  register "gemini-srv"
    [
      main $ default_random $ static $ certs $ default_posix_clock
      $ generic_stackv4v6 default_network
      $ default_time;
    ]
