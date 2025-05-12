include Mehari.FS with module IO := Lwt and type dir_path := string

val run_cgi : ?timeout:float -> ?non_parsed:bool -> string -> handler
