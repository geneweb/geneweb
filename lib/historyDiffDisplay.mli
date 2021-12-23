
(** Displays the history list associated to the history file in argument *)
val print_clean : Config.config -> unit

(** Cleans the history associated to the history file in argument *)
val print_clean_ok : Config.config -> unit

(** Intepretation of the template file updhist_diff.txt *)
val print : Config.config -> Gwdb.base -> unit
