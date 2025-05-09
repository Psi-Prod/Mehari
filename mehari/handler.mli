module Make (IO : Signatures.IO) : sig
  type t = Request.t -> Response.t IO.t
end
