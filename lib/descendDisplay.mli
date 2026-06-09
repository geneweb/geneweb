(* Copyright (c) 1998-2007 INRIA *)

(** Descendant rendering for the [m=D] request.

    Entry points consumed by gwd (request dispatch) and by the [v7_descend]
    plugin. {!print} is the dispatcher; the other values render one specific [t]
    mode and are exported because the plugin calls them directly. *)

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

val print_tree :
  Config.config ->
  Geneweb_db.Driver.base ->
  int ->
  Geneweb_db.Driver.person ->
  unit
(** Displays a centred tree of descendants. *)

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
    - "N": {!display_descendants_with_numbers};
    - "G": {!display_descendant_index};
    - "C": {!display_spouse_index};
    - "T": {!print_tree};
    - "TV": compact descendants tree (Jean Vaucher layout).

    The [v] parameter sets the maximum depth (1 = children, 2 = grandchildren,
    …). When [t] is absent, the customisation form is displayed. *)
