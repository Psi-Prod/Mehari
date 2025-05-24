(** CGI implementation. *)

include Mehari.CGI with module IO := Lwt
