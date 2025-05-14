(** Static file serving. *)

module Make (Fs : Signatures.FILE_SYSTEM) :
  Signatures.STATIC with module IO := Fs.IO and type dir_path := Fs.path
