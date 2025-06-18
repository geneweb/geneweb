(* Public functions for API (plugin v7_descend) *)

val display_descendants_level :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays only descendants for specified level in unordered lists *)

val display_descendants_with_numbers :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays descendants with numerated by letter list. Title links to
    descendats index *)

val display_descendant_index :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays index of descendants *)

val display_spouse_index :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays index of descendant's spouses *)

val display_descendant_with_table :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays descendants in the table where rows are ordered by D'Aboville
    number. *)

val print_tree :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays tree of descendants *)

val print_aboville :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays descendants as follows :

    person | desc1 | desc2 | | desc21 | desc3 *)

val desmenu_print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Prints form that allows to customise display of descendants *)

val print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Displays the descendants of the selected in [conv.env] person. Descendants
    could be displayed by different ways depending on variable {i t} in
    [conv.env] environement:

    - "L" dispalying descendants in unordered list
    - "F" same as "L" but displays only female line
    - "M" same as "L" but displays only female line
    - "H" table dispalying
    - "I" table dispalying with spouses information
    - "A" numerated list (d'Aboville)
    - "V" displaying a tree of descendants

    Previous dispalyings are done by template evaluation. Next ones are done by
    functions inside this module:

    - "B" for [print_aboville]
    - "S" for [display_descendants_level]
    - "K" for [display_descendant_with_table]
    - "N" for [display_descendants_with_numbers]
    - "G" for [display_descendant_index]
    - "C" for [display_spouse_index]
    - "T" for [print_tree]

    Variable {i v} is used to select maximal level to descend for descendant
    displaying (1 for children, 2 for grandchildren, etc). If {i t} variable
    isn't defined, then displays the form that allows customising of display. *)
