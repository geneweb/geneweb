
(** Displays the descendants of the selected person *)
val print : Config.config -> Gwdb.base -> Gwdb.person -> unit

(* Public functions for API (plugin v7_descend) *)
val display_descendants_level :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val display_descendants_with_numbers :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val display_descendant_index :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val display_spouse_index :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val display_descendant_with_table :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val print_tree :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val print_aboville :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> unit

val desmenu_print :
  Config.config -> Gwdb.base -> Gwdb.person -> unit

