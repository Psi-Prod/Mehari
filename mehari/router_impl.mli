(** Routeur functorized implementation. *)

module Make : (RateLimiter : Signatures.RATE_LIMITER) (IO : Signatures.IO) ->
  Signatures.ROUTER with module IO = IO and type rate_limiter := RateLimiter.t
