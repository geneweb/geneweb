val print_clean : Config.config -> unit
(** Displays page that allows to select all revision of the history file in
    argument that user may want to clean *)

val print_clean_ok : Config.config -> unit
(** Cleans the history associated to the history file in argument *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays the page that allows to select (with variable {i t} = "SUM") and to
    view (with variable {i t} = "DIFF") the difference between all revisions of
    history file of concerned person in variable {i f}. Intepretate the template
    file {i updhist_diff.txt} *)
