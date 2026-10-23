(** Robot file layout shared by gwd and gwrobot. Marshal does not check types:
    both programs must read and write these exact definitions. *)

val magic_robot : string
(** Header of the robot file. Must change whenever a type below changes. *)

val file : unit -> string
(** [file ()] is the robot file, [cnt/robot] in the bases directory, shared by
    all the bases served by gwd. *)

val lock_file : unit -> string
(** [lock_file ()] is the lock guarding {!file}; its directory is created if
    needed. Unlike [gwd.lck], which is per base in reorg mode, it is shared by
    all the bases. *)

module W : Map.S with type key = string
(** Maps keyed by client address. *)

(** Access level of a request; friends and wizards carry the user name. *)
type norfriwiz = Normal | Friend of string | Wizard of string

type who = {
  acc_times : float list;
  oldest_time : float;
  nb_connect : int;
  nbase : string;
  utype : norfriwiz;
}
(** A monitored client, over the window passed to [Robot.check]:
    - [acc_times]: times of its requests in the window, most recent first;
    - [oldest_time]: start of the series counted in [nb_connect], shown in the
      block log;
    - [nb_connect]: number of requests in the window;
    - [nbase], [utype]: base and access level of its last request. *)

type excl = {
  mutable excl : (string * int ref) list;
  mutable who : who W.t;
  mutable max_conn : int * string;
  mutable last_summary : float;
}
(** Content of the robot file:
    - [excl]: blocked addresses or patterns ([146.174.*]) with the number of
      refused attempts;
    - [who]: monitored clients;
    - [max_conn]: highest [nb_connect] seen and its address, reset when a client
      is blocked;
    - [last_summary]: unused, kept for compatibility with existing files. *)

val empty : unit -> excl
(** [empty ()] returns fresh robot data with no blocked or monitored client. *)

val output_excl : out_channel -> excl -> unit
(** [output_excl oc xcl] writes [xcl] to [oc] in the robot file format,
    {!magic_robot} header included. *)
