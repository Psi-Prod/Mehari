module Make (I : sig
  type 'a t
end) =
struct
  type t = Request.t -> Response.t I.t
end
