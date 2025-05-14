(** Rate limiter functorized implementation. *)

module Make (Clock : Signatures.PCLOCK) (IO : Signatures.IO) :
  Signatures.RATE_LIMITER with module IO = IO and type clock = Clock.t
