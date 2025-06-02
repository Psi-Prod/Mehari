(** CGI implementation. *)

include Mehari.CGI with module IO := Identity_monad
