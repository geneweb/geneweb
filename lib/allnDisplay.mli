val print_surnames : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays all persons surnames present in the base. Display could be
    different depending on environement [conf.env]. These variables affect the
    display:

    - tri : "F" to display surnames by frequency, "S" to display surnames
      regrouped by first letter (depends on variable "k") otherwsise display
      surnames just ordered alphabeticaly
    - k : Defines common prefix for surnames (empty for all)
    - o : "A" to print all surnames (if less then [Alln.default_max_cnt])
      otherwise prints links to access different type of displaying *)

val print_first_names : Config.config -> Geneweb_db.Driver.base -> unit
(** Same as [print_surnames] but dealing with first names. *)
