(** A module handling robots requests *)
(* S: This module seems obsolete *)

module W : Map.S with type key = string
(** Keyed by IP alone: the detection window and the ban decision are site-wide,
    so an abusive IP is caught and blocked across every database served by this
    gwd instance, not just the one it happens to be hitting. *)

type norfriwiz = Normal | Friend of string | Wizard of string

type who = private {
  acc_times : float list;  (** The timings of the connexion attempts *)
  oldest_time : float;
      (** The first connection in the specified window (check option -robot-xcl)
          of time in which successive connections are attempted. *)
  nb_connect : int;  (** The number of connection in the specified window. *)
  nbase : string;
      (** The base this IP was last seen on. Reporting only: it plays no role in
          the threshold or ban decision, which stays site-wide. *)
  utype : norfriwiz;  (** The kind of robot *)
}

type excl = {
  mutable excl : (string * int ref) list;
  mutable who : who W.t;
  mutable max_conn : int * string;  (** (count, ip) *)
  mutable last_summary : float;
}
(** A collection of robots: the list contains forbidden robots and the map
    contains accepted (under conditions) robots. *)

val robot_error : Geneweb.Config.config -> int -> int -> 'a
(** Prints an error "Access refuned" in HTML and raises an `Exit` exception. *)

val robot_excl : unit -> excl * string
(** Reads the content of the admin file managing robots and returns its content
    and the full file name. *)

val save : excl -> string -> unit
(** [save xcl fname] persists [xcl] to [fname] (typically the file name returned
    by {!robot_excl}), using the exact on-disk format {!robot_excl} reads. This
    is the single, canonical write path for the robot-ban state, used both by
    {!check} and by external tools (e.g. the [gwrobot] CLI) so that no other
    module ever needs its own copy of {!excl}/{!who} to read or write this file
    safely. *)

val lock_file : unit -> string
(** The file used to serialize access to the (deliberately global, server-wide)
    robot-ban state read/written by {!robot_excl} and {!check}. This is
    intentionally independent from [GWPARAM.adm_file], which in "reorg" mode is
    scoped per database: the robot state must stay protected by a single,
    site-wide lock regardless of which base a given request targets. *)

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
