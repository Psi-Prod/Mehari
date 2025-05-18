type 'a t = 'a

val return : 'a -> 'a
val bind : 'a -> ('a -> 'b) -> 'b
val map : ('a -> 'b) -> 'a -> 'b
