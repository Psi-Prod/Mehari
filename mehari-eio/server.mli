(** Server implementation. *)

include
  Mehari.SERVER
    with type config := Config.t
     and module IO := Identity_reader_monad
