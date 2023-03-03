open Gwdb
open Config

val cousins_t :
  (Gwdb_driver.iper * Gwdb_driver.ifam list * Gwdb_driver.iper * int list) list
  array
  array
  option
  ref

val cousins_dates_t : (int * int) array array option ref

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

(* Functions to obtain info about cousins of a person *)

(* The various functions are typically called with two parameters:
    - l1 : number of generations up
    - l2 : number of generations down
    0, 0 is myself
    1, 1 is my brothers ans sisters
    2, 1 is my uncle and aunts
    2, 2 is my cousins
    etc
*)

val init_cousins_cnt :
  base ->
  int ->
  (* max ancestors level *)
  int ->
  (* max descendants level *)
  person ->
  (Gwdb_driver.iper * Gwdb_driver.ifam list * Gwdb_driver.iper * int list) list
  array
  array
  * (int * int) array array
(** initialise
  - a 2D array of lists of cousins at l1, l2
    for each cousin, record 
      - his family, 
      - his parent (through which he is reached),
      - the level (possibly multiple levels dut to implex)
  - a 2D array of tuples (min, max) for dates of cousins at l1, l2
*)

val max_l1_l2 : base -> int -> int -> person -> int * int
(** obtain the (min, max) value for cousins at l1, l2 *)

val cousins_l1_l2_aux :
  base ->
  int ->
  (* max ancestors level *)
  int ->
  (* max descendants level *)
  string ->
  (* up l1 generations *)
  string ->
  (* down l2 generations *)
  person ->
  (Gwdb_driver.iper * Gwdb_driver.ifam list * Gwdb_driver.iper * int list) list
  option

val cousins_fold :
  (Gwdb_driver.iper * Gwdb_driver.ifam list * Gwdb_driver.iper * int list) list ->
  (Gwdb_driver.iper
  * (Gwdb_driver.ifam list * Gwdb_driver.iper list * int)
  * int list)
  list
(** create a new list of (ip, (ifaml, iancl, cnt), lev) from list of (ip, ifaml, ipar, lev)
  The effect is to assemble multiple items under a single ip
*)

val anc_cnt_aux :
  base ->
  int ->
  (* max anc level *)
  int ->
  (* level *)
  bool ->
  (* up to ot at *)
  person ->
  (Gwdb_driver.iper * Gwdb_driver.ifam list * Gwdb_driver.iper * int list) list
  option
(** Get the list of ancestors up to or at level *)

val desc_cnt_aux :
  base ->
  int ->
  (* max desc level *)
  int ->
  (* level *)
  bool ->
  (* up to ot at *)
  person ->
  (Gwdb_driver.iper * Gwdb_driver.ifam list * Gwdb_driver.iper * int list) list
  option
(** Get the list of descendants up to or at level *)
