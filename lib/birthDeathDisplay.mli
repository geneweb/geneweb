
(** Lists the last births *)
val print_birth : Config.config -> Gwdb.base -> unit

(** Lists the last deaths *)
val print_death : Config.config -> Gwdb.base -> unit

(** Lists the persons who lived the longest *)
val print_longest_lived : Config.config -> Gwdb.base -> unit

(** Displays the list of the oldest persons that are still alive or, if unknown,
    whose death are not probable *)
val print_oldest_alive : Config.config -> Gwdb.base -> unit

(** Lists the last marriages *)
val print_marriage : Config.config -> Gwdb.base -> unit

(** Displays the list of the oldest couples that still exist *)
val print_oldest_engagements : Config.config -> Gwdb.base -> unit

(** Displays several links for statistics: latest births, death, marriages, the
    oldest couples, persons that are alive and who lived the longest, as well as
    a population pyramid *)
val print_statistics : Config.config -> unit

(** Displays a population pyramid from the base data *)
val print_population_pyramid : Config.config -> Gwdb.base -> unit
