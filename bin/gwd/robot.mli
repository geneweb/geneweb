(** A module handling robots requests *)
(* S: This module seems obsolete *)

include module type of struct
  include Geneweb_robot
end

val robot_error : Geneweb.Config.config -> int -> int -> 'a
(** Prints an error "Access refuned" in HTML and raises an `Exit` exception. *)

val robot_excl : unit -> excl * string
(** Reads the content of the admin file managing robots and returns its content
    and the full file name. *)

val min_disp_req : int ref

val summary : unit -> string
(** [summary ()] describes the robot file in one line: number of blocked and
    monitored addresses, and the most active one. *)

val check :
  float ->
  string ->
  int ->
  int ->
  Geneweb.Config.config ->
  bool ->
  int * int * int * (string * float) list
(** [check tm from max_call sec conf suicide] Returns a tuple containing: * the
    number of robots who attempted to connect twice * the number of wizard
    robots who attempted to connect twice * the number of friend robots who
    attempted to connect twice * the wizards list and their last connection
    attempt. It also updates the robot file by blocking robots who did too many
    attempts. *)
