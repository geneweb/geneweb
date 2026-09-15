type event =
  | Wait_object of int
  | Wait_abandoned of int
  | Wait_timeout
  | Wait_failed

external wait_for_multiple_objects : int array -> bool -> int -> event
  = "geneweb_wait_for_multiple_objects"
