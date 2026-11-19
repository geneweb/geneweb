(** Periodic statistics of a base, appended by gwd to [<base>_stats.txt] in the
    counter directory of the base ({!Geneweb.GWPARAM.cnt_d}).

    Enabled by [stats_period=n] in the [.gwf] file of the base, [n] being the
    period in minutes; absent or [0] disables it.

    gwd has no timer: at the end of each request, {!record} adds the request
    duration and byte counts to an accumulator ([<base>_qtime.txt], shared by
    the worker processes) and, once the period has elapsed since the last
    snapshot, appends a new one. Without any request, no snapshot is taken.

    Each snapshot is a tab-separated line (a header line is written when the
    file is created): date (UTC, ISO 8601), timestamp, number of persons
    (without "? ?"), number of families, connected users, wizards and friends
    (see {!Geneweb.Config.connections}), robot window (seconds), number of timed
    requests since the previous snapshot, their mean and maximum durations
    (seconds), bytes received and sent since the previous snapshot. Bytes are
    those exchanged by gwd itself (behind a reverse proxy, between the proxy and
    gwd); static files served outside a base request (css, js, fonts) are not
    counted. *)

val record :
  Geneweb.Config.config ->
  window:int ->
  bytes_in:int ->
  bytes_out:int ->
  float option ->
  unit
(** [record conf ~window ~bytes_in ~bytes_out duration] records a request on the
    base of [conf] that received [bytes_in] and sent [bytes_out] bytes;
    [duration] is [None] for a request not included in the durations (binary
    content). Takes a snapshot if one is due. [window] is the robot observation
    window, in seconds. Does nothing when the statistics are disabled or the
    base does not exist; errors are ignored. *)

val stats_file : Geneweb.Config.config -> string
(** [stats_file conf] is the path of the snapshot file of the base of [conf]. *)
