type t = {
  ip : Ipaddr.Prefix.t;
  backlog : int;
  reuseaddr : bool;
  reuseport : bool;
}

let make ?(reuseport = true) ?(reuseaddr = true) ?(backlog = 64)
    ?(ip = Ipaddr.(V4 (V4.Prefix.make 8 V4.localhost))) () =
  { ip; backlog; reuseaddr; reuseport }
