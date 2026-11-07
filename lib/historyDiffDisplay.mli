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

val print_change :
  Config.config ->
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.ifam,
    string )
  Def.base_changed ->
  unit
(** [print_change conf base change] Prints the differences made by [change] to
    its main person with the template {i updhist_change.txt}, also used by the
    revision history page. Prints nothing if there is none. Does not depend on
    the history being enabled. *)
