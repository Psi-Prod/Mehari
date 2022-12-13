module Make (IO : Io.S) = struct
  type t = Request.t -> Response.t IO.t
end
