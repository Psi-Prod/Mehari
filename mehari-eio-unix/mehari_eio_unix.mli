(** {1 Net} *)

(** @closed *)
include
  Mehari.UNIX
    with module IO := Mehari_eio.Direct
     and type addr = Mehari_eio.Addr.t
     and type dir_path := Eio.Fs.dir Eio.Path.t
