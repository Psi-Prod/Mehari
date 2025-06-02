(** File system features implementation. *)

val read_chunks : Miou_unix.file_descr -> string Seq.t
(** [read_chunks fd] reads a seq of chunks from [fd]. *)

include Mehari.FS with module IO := Identity_monad and type path := string
