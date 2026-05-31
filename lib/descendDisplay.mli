(* Copyright (c) 1998-2007 INRIA *)

(** Descendant rendering for the [m=D] request.

    Entry points consumed by gwd (request dispatch) and by the [v7_descend]
    plugin. {!print} is the dispatcher; the other values render one specific [t]
    mode and are exported because the plugin calls them directly. *)

val display_descendants_level :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays the descendants of a single generation as an unordered list. *)

val display_descendants_with_numbers :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays descendants as a letter-numbered list; the title links to the
    descendants index. *)

val display_descendant_index :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays the index of descendants. *)

val display_spouse_index :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays the index of the descendants' spouses (themselves not descendants).
*)

val display_descendant_with_table :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays descendants in a table whose rows are ordered by d'Aboville number.
*)

val print_tree :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays a centred tree of descendants. *)

val print_aboville :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays descendants in indented d'Aboville form, e.g.:

    person | desc1 | desc2 | | desc21 | desc3 *)

val desmenu_print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Prints the form used to customise the descendants display. *)

val print :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Displays the descendants of the person selected through [conf.env]. The
    rendering depends on the [t] parameter.

    Template-rendered modes:
    - "L": descendants as an unordered list;
    - "F": as "L", restricted to the female line;
    - "M": as "L", restricted to the male line;
    - "D": descendants list, [deslist_hr] template;
    - "H": table;
    - "I": table including spouse information;
    - "A": table ordered by d'Aboville number;
    - "V": descendants tree.

    Modes rendered by the functions of this module:
    - "B": {!print_aboville};
    - "S": {!display_descendants_level};
    - "K": {!display_descendant_with_table};
    - "N": {!display_descendants_with_numbers};
    - "G": {!display_descendant_index};
    - "C": {!display_spouse_index};
    - "T": {!print_tree};
    - "TV": compact descendants tree (Jean Vaucher layout).

    The [v] parameter sets the maximum depth (1 = children, 2 = grandchildren,
    …). When [t] is absent, the customisation form is displayed. *)
