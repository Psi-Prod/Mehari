module type ADDR = sig
  type t

  val compare : t -> t -> int
  val pp : Stdlib.Format.formatter -> t -> unit
end

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type T = sig
  type t
end
