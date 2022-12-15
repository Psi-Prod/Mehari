module Make (Clock : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) :
  Mehari.NET with module IO = Lwt and type stack = Stack.t
