module type S = sig
  module IO : Types.IO

  val get_certs :
    exn_msg:string -> Tls.Config.certchain list -> Tls.Config.own_cert IO.t
end

module Make (IO : Types.IO) : S with module IO := IO = struct
  let get_certs ~exn_msg = function
    | c :: tl -> `Multiple_default (c, c :: tl) |> IO.return
    | _ -> invalid_arg exn_msg
end
