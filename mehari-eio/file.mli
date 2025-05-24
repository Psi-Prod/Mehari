(** File system features implementation. *)

include
  Mehari.FS
    with module IO := Identity_reader_monad
     and type path := [ `Dir ] Eio.Path.t
