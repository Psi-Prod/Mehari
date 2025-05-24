(** File system features implementation. *)

include Mehari.FS with module IO := Lwt and type dir_path := string
