val print_birth : Config.config -> Geneweb_db.Driver.base -> unit
(** Lists the last births *)

val print_death : Config.config -> Geneweb_db.Driver.base -> unit
(** Lists the last deaths *)

val print_longest_lived : Config.config -> Geneweb_db.Driver.base -> unit
(** Lists the persons who lived the longest *)

val print_oldest_alive : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the list of the oldest persons that are still alive or, if unknown,
    whose death are not probable *)

val print_marriage : Config.config -> Geneweb_db.Driver.base -> unit
(** Lists the last marriages *)

val print_oldest_engagements : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the list of the oldest couples that still exist *)

val print_statistics : Config.config -> unit
(** Displays several links for statistics: latest births, death, marriages, the
    oldest couples, persons that are alive and who lived the longest, as well as
    a population pyramid *)

val print_population_pyramid : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a population pyramid from the base data *)
