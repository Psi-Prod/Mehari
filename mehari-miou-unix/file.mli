(** File system features implementation. *)

include Mehari.FS with module IO := Identity_monad and type path := string
