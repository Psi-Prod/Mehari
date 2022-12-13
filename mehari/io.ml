module type S = sig
  type 'a t

  val return : 'a -> 'a t
end
