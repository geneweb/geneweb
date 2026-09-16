module Wait : sig
  type event =
    | Wait_object of int
    | Wait_abandoned of int
    | Wait_timeout
    | Wait_failed

  val wait_for_multiple_objects : int array -> bool -> int -> event
end
