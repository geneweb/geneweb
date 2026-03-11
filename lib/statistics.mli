(** Comprehensive database statistics as JSON.

    Handles [m=STATS]: computes all statistics in two O(N) passes (persons then
    families) and emits the result as a single JSON document with Content-type
    [application/json]. *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** [print conf base] computes and outputs all statistics as JSON. *)
