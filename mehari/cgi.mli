(** CGI implementation as defined by RFC3875.

    @see < https://www.rfc-editor.org/rfc/rfc3875 > *)

type t

val make : ?server_addr:Ipaddr.t -> script_path:string -> Request.t -> t
(** [make ?server_addr ~script_path req] creates a CGI environment. *)

val to_env : t -> (string * string) array
(** [to_env cgi_env] returns an array of pair of form [(name, value)]. *)
