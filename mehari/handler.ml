module Make (IO : Io.S) = struct
  type 'addr t = 'addr Request.t -> Response.t IO.t
end
