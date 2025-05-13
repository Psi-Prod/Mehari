type env = Eio_unix.Stdenv.base
type 'a t = env -> 'a

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
