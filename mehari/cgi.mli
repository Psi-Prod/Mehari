(** CGI implementation as defined by RFC3875.

    @see < https://www.rfc-editor.org/rfc/rfc3875 > *)

type t

val make : Request.t -> script_path:string -> server_addr:Ipaddr.t -> t
(** [make req ~script_path ~server_addr] creates a CGI environment. *)

val to_env : t -> (string * string) array
(** [to_env cgi_env] returns an array of pair of form [(name, value)]. *)
