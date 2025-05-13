type env = Eio_unix.Stdenv.base
type 'a t = env -> 'a

let return x _ = x
let bind reader f env = f (reader env) env
let map f reader env = (return (f (reader env))) env
