open Gwdb
open Config

val default_max_cnt : int
(** Default number of relatives that could be listed at the same page *)

val max_cousin_level : config -> base -> person -> int

val children_of_fam : base -> ifam -> iper list
(** Retruns list of children of the giving family *)

val siblings : config -> base -> iper -> (iper * (iper * Def.sex)) list
(** Returns list of person's siblings that includes also half-blood siblings. Every sibling
    is annotated with parent's id and parent's sex. For common father's and mother's
    children father's annotation is preserved. *)

val has_desc_lev : config -> base -> int -> person -> bool
(** [has_desc_lev conf base lev p] tells if person [p] has descendants at the level [lev].
    [lev] 2 represents his children, 3 represents grandchildren, etc. *)

val br_inter_is_empty : ('a * 'b) list -> ('a * 'c) list -> bool
(** Tells if two family branches don't itersect *)

val sibling_has_desc_lev : config -> base -> int -> iper * 'a -> bool
(** Same as [has_desc_lev] but used for a person's sibling as returned by [siblings]. *)

(* -- TODO WIP functions moved from Perso -- *)
(* TODO better type for big tuples *)

val get_descendants_at_level : base -> person -> int -> iper list

val ascendants :
  base ->
  (iper * 'a list * iper * int list) list ->
  (iper * 'b * 'c * int list) list ->
  'd ->
  (iper * 'a list * iper * int list) list

val anc_cnt_aux :
  base ->
  'a ->
  int ->
  bool ->
  person ->
  (iper * ifam list * iper * int list) list option

val desc_cnt_aux :
  base ->
  'a ->
  int ->
  bool ->
  person ->
  (iper * ifam list * iper * int list) list option

val cousins_fold :
  (iper * 'a list * 'b * int list) list ->
  (iper * ('a list * 'b list * int) * int list) list

val cous_paths_dates_aux :
  'a -> base -> 'b -> person -> string -> string -> bool -> 'c

val cousins_l1_l2_aux :
  'a ->
  base ->
  'b ->
  string ->
  string ->
  person ->
  (iper * ifam list * iper * int list) list option

val max_l1_l2 : 'a -> base -> 'b -> person -> int * int
