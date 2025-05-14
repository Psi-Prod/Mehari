(** Routeur functorized implementation. *)

module Make : (RateLimiter : Signatures.RATE_LIMITER)
  (Logger : Signatures.LOGGER)
  ->
  Signatures.ROUTER
    with module IO = RateLimiter.IO
     and type rate_limiter := RateLimiter.t
