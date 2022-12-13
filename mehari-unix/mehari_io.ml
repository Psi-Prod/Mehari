module Stack = Tcpip_stack_socket.V4V6
include Mehari_mirage.Make (Pclock) (Stack)
