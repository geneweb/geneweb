(** Anomalies pages of gwd (wizard only: the caller wraps them in [w_wizard]).

    - [m=ANOMA]: summary of the report written by gwc, ged2gwb or anoma
      ({!Anomaly}): per kind, the anomalies found, verified and left to check;
      extreme ages; persons with several kinds of anomalies.
    - [m=ANOMA&k=<kind>]: the anomalies of one kind, 100 per page ([pg]), with
      their message in the reader's language, links to the persons and to
      their edit forms, and a "verified" button for the kinds that can be
      verified. Filters: [wiz] last wizard to modify a person, [amin] and
      [amax] age, [all=1] also the verified ones.
    - [m=OK_ADD&a=<anomaly>] / [m=OK_DEL&a=<anomaly>]: verify an anomaly, or
      cancel its verification ({!AnomalyWhitelist.record}); [a] is the
      anomaly's JSON. Plain text answer, HTTP 400 on bad input.
    - [m=OK_FILE]: the verified anomalies, most recent first, with who verified
      them and when.
    - [m=OK_FOLD]: merge the pending verifications.

    Kinds listed in the [anomalies_hidden] base parameter (comma-separated
    kind names) are shown apart, under "other checks". *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
val print_ok_add : Config.config -> Geneweb_db.Driver.base -> unit
val print_ok_del : Config.config -> Geneweb_db.Driver.base -> unit
val print_ok_file : Config.config -> Geneweb_db.Driver.base -> unit
val print_ok_fold : Config.config -> Geneweb_db.Driver.base -> unit
