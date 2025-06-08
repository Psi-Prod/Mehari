(** Logger functorized implementation. *)

val src : Logs.src
(** Mehari's logs source. *)

module Make : (Clock : Signatures.PCLOCK)
  (IO : sig
     include Signatures.IO

     val finally : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
   end)
  -> Signatures.LOGGER with module IO = IO and type clock = Clock.t
