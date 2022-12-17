module type ADDR = sig
  type t

  val compare : t -> t -> int
  val pp : Stdlib.Format.formatter -> t -> unit
end