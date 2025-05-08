module Make (IO : Types.IO) = struct
  type t = Request.t -> Response.t IO.t
end
