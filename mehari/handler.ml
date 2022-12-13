type t = Request.t -> Response.t Lwt.t

module Make (I : sig
  type 'a t
end) =
struct
  type t = Request.t -> Response.t I.t
end

type lwt_handler = Make(Lwt).t
