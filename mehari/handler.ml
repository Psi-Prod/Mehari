module Make (IO : Types.IO) = struct
  type 'addr t = 'addr Request.t -> Response.t IO.t
end
