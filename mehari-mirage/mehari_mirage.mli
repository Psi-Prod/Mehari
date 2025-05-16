(** Mirage OS compatiblity. *)

module type S = Interface.S
(** Describe a Mirage OS server signature. *)

(** A functor building an IO module from Mirage components. *)
module Make
    (Clock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S) : S with type stack = Stack.t
