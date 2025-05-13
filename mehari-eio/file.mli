include
  Mehari.FS
    with module IO := Identity_reader_monad
     and type dir_path := [ `Dir ] Eio.Path.t
