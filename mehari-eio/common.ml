module Direct = struct
  type 'a t = 'a

  let return x = x
  let bind x f = f x
  let map f x = f x
end

module Addr = struct
  type t = Eio.Net.Ipaddr.v4v6

  let compare = Stdlib.compare
  let pp = Eio.Net.Ipaddr.pp
end
