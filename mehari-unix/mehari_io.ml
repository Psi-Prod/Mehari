module Stack = Tcpip_stack_socket.V4V6
include Mehari.Mirage.Make (Pclock) (Stack)
