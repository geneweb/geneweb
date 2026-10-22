(** Robot detection for gwd. Every request goes through {!check}, which records
    the client in the robot file ([cnt/robot] in the bases directory), blocks
    clients exceeding the [--ban-threshold] limits and counts the connected
    users shown in the footer. *)

include module type of struct
  include Geneweb_robot
end

val robot_error : Geneweb.Config.config -> int -> int -> 'a
(** [robot_error conf max_call sec] sends a 403 page from the [robot] template,
    with [max_call] and [sec] as its [cnt] and [sec] variables, then raises
    [Exit]. *)

val robot_excl : unit -> excl * string
(** [robot_excl ()] reads the robot file and returns its content with its path.
    A file in the previous [GWRB0007] format is migrated; a missing or
    unreadable file gives empty data. *)

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
(** [check tm from max_call sec conf suicide] records a request made at time
    [tm] by the client address [from], then saves the robot file.

    The client is blocked when it made more than [max_call] requests within the
    last [sec] seconds, or when [suicide] is set (the request has a [suicide]
    parameter). A blocked client, or one matching a blocked pattern, gets
    {!robot_error}, which raises [Exit].

    Otherwise, returns for the base of [conf]:
    - the number of connected users: wizards, plus friends and visitors with
      more than two requests within the window;
    - the number of distinct wizards;
    - the number of friends with more than two requests within the window;
    - each wizard with the time of their last request. *)
