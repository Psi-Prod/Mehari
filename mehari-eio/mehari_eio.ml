module IO = Direct
module RateLimiter = Mehari.Private.Rate_limiter_impl.Make (Pclock) (Direct)

module Logger =
  Mehari.Private.Logger_impl.Make
    (Pclock)
    (struct
      include Direct

      let finally t f r = try f (t ()) with exn -> r exn
    end)

module Router = Mehari.Private.Router_impl.Make (RateLimiter) (Logger)
module Server = Server_impl.Make (Logger)
