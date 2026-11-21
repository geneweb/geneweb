(** [m=GWD_STATS]: charts of the periodic statistics of a base (see
    {!StatsLog}), for wizards.

    Without [json], prints the page (template [gwdstats]), which fetches the
    data with [json=1]. Parameters:
    - [p]: [day] (default), [week] (Monday to Sunday) or [month];
    - [d]: a day of the period, [YYYY-MM-DD], in the time zone of the server;
      default today.

    The day view has one point per snapshot; the week view aggregates them by
    hour, the month view by day. Connections are averaged over the snapshots of
    a point (with their maximum); request rates and traffic are computed over
    the intervals between snapshots, whose length varies since a snapshot is
    taken by the first request after the period; response times are weighted by
    the number of requests. Missing points are [null]. The JSON also gives the
    all-time record of connections ([record]: value, day and time of the first
    snapshot reaching it), over the whole file. *)

val print : Geneweb.Config.config -> unit
