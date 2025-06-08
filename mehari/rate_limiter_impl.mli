(** Rate limiter functorized implementation. *)

module Make (Clock : Signatures.PCLOCK) :
  Signatures.RATE_LIMITER with type clock = Clock.t
