(** Server implementation. *)

open Mehari

module Make : (Logger : Private.Signatures.LOGGER) ->
  SERVER with type config := Config.t and module IO := Identity_reader_monad
