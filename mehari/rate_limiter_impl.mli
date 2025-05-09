module type S = sig
  module IO : Signatures.IO

  type t
  type clock

  val check : t -> Request.t -> Response.t IO.t option

  val make :
    clock -> ?period:int -> int -> [ `Second | `Minute | `Hour | `Day ] -> t
end

module Make (Clock : Signatures.PCLOCK) (IO : Signatures.IO) :
  S with module IO = IO and type clock = Clock.t
