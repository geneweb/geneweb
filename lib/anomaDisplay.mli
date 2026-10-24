open Config

(** Anomalies report produced by [anoma], and its whitelist of verified
    anomalies, served through gwd (modes [m=ANOMA], [m=OK_ADD], [m=OK_FILE],
    [m=OK_FOLD]).

    All files live in [<bases_dir>/<base>.gwb/config/], the layout [anoma]
    writes:
    - [anomalies.html]: the report;
    - [whitelist_warnings.txt]: the consolidated whitelist;
    - [whitelist_d/<wizard>.ok]: per-wizard contributions not yet folded into
      the whitelist.

    Every function here expects wizard rights; the caller (see
    [Request.treat_request]) wraps them in [w_wizard]. *)

val print_report : config -> Geneweb_db.Driver.base -> unit
(** [m=ANOMA]: serve [anomalies.html] as is, same-origin with the base so that
    the wizard session reaches its "Submit to base" button. The viewing wizard's
    login is injected for the report's "me" filter. *)

val print_ok_add : config -> Geneweb_db.Driver.base -> unit
(** [m=OK_ADD&key=<person or couple>&code=<M|B|D|E|CO|EO>]: append one verified
    entry ["key: code"] to the current wizard's own fragment. Each wizard writes
    a different file, so concurrent submissions need no locking. Answers in
    plain text, with HTTP 400 on invalid input, so that the report's script
    counts rejected entries as failures. *)

val print_ok_file : config -> Geneweb_db.Driver.base -> unit
(** [m=OK_FILE]: read-only view of the consolidated whitelist and of the pending
    fragments. Never editable: hand edits would race with [anoma]'s fold and
    with wizard submissions. *)

val print_ok_fold : config -> Geneweb_db.Driver.base -> unit
(** [m=OK_FOLD]: run [anoma <base> -bd <bases_dir>] (no [-in]: consolidate
    only), which folds the fragments into the whitelist and archives them, then
    show its output. [anoma] is looked up next to the running gwd. *)
