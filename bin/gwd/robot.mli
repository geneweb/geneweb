(** A module handling robots requests *)
(* S: This module seems obsolete *)

val magic_robot : string

module W : Map.S with type key = string

type norfriwiz = Normal | Friend of string | Wizard of string

type who = private {
  acc_times : float list;  (** The timings of the connexion attempts *)
  oldest_time : float;
      (** The first connection in the specified window (check option -robot-xcl)
          of time in which successive connections are attempted. *)
  nb_connect : int;  (** The number of connection in the specified window. *)
  nbase : string;  (** Always be equal to conf.bname *)
  utype : norfriwiz;  (** The kind of robot *)
}

type excl = {
  mutable excl : (string * int ref) list;
  mutable who : who W.t;
  mutable max_conn : int * string;
}
(** A collection of robots: the list contains forbidden robots and the map
    contains accepted (under conditions) robots. *)

val robot_error : Geneweb.Config.config -> int -> int -> 'a
(** Prints an error "Access refuned" in HTML and raises an `Exit` exception. *)

val robot_excl : unit -> excl * string
(** Reads the content of the admin file managing robots and returns its content
    and the full file name. *)

val min_disp_req : int ref

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
