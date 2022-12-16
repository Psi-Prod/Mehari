let main net = Mehari_eio.Server.run net (fun req -> req)
let () = Eio_main.run (fun env -> Eio.Stdenv.net env |> main)
