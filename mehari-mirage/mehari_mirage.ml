open Mehari

module Runtime = struct
  type stack = Mnet.TCP.state
  type listener = Mnet.TCP.listen

  let listen = Mnet.TCP.listen

  module TLS = struct
    include Mnet_tls

    let peer flow = file_descr flow |> Mnet.TCP.peers |> snd

    let really_read fd ?off ?len buf =
      try Ok (really_read fd ?off ?len buf) with
      | Tls_alert a -> Error (`Tls_alert a)
      | Tls_failure f -> Error (`Tls_failure f)

    let write flow ?off ?len s =
      try Ok (write flow ?off ?len s)
      with Closed_by_peer -> Error `Connection_closed

    let close flow = Mnet_tls.shutdown flow `read_write
  end

  module TCP = struct
    type t = Mnet.TCP.flow

    let accept = Mnet.TCP.accept
    let tls_upgrade config flow = Mnet_tls.server_of_fd config flow
  end

  let now = Mirage_ptime.now
  let sleep secs = Mkernel.sleep (Int.of_float (1_000_000_000. *. secs))
end

include Server.Make (Runtime)
