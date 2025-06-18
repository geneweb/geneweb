open Config

type one_cousin =
  Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int

type cousins_i_j = one_cousin list

val cousins_table : cousins_i_j array array
val cousins_dates_t : (int * int) array array option ref

val default_max_cnt : int
(** Default number of relatives that could be listed at the same page *)

val mal : int
(** max value of max_ancestor_level *)

val mdl : int
(** max value of max_descendant_level *)

val max_cousin_level : config -> int

val max_ancestor_level :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int -> int

val max_descendant_level :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int -> int

val children_of_fam :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.iper list
(** Retruns list of children of the giving family *)

val siblings :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  (Geneweb_db.Driver.iper * (Geneweb_db.Driver.iper * Def.sex)) list
(** Returns list of person's siblings that includes also half-blood siblings.
    Every sibling is annotated with parent's id and parent's sex. For common
    father's and mother's children father's annotation is preserved. *)

val has_desc_lev :
  config -> Geneweb_db.Driver.base -> int -> Geneweb_db.Driver.person -> bool
(** [has_desc_lev conf base lev p] tells if person [p] has descendants at the
    level [lev]. [lev] 2 represents his children, 3 represents grandchildren,
    etc. *)

val br_inter_is_empty : ('a * 'b) list -> ('a * 'c) list -> bool
(** Tells if two family branches don't itersect *)

val sibling_has_desc_lev :
  config -> Geneweb_db.Driver.base -> int -> Geneweb_db.Driver.iper * 'a -> bool
(** Same as [has_desc_lev] but used for a person's sibling as returned by
    [siblings]. *)

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
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int)
  list
  array
  array
  * (int * int) array array
(** initialise
    - a 2D array of lists of cousins at l1, l2 for each cousin, record
    - his family,
    - his parent (through which he is reached),
    - the level (possibly multiple levels dut to implex)
    - a 2D array of tuples (min, max) for dates of cousins at l1, l2 *)

val min_max_date :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  string ->
  string ->
  int option
(** for cousins_dates.(l1).(l2) determine min or max date *)

val max_l1_l2 :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> int * int
(** determine non empty max ancestor level (l1) and non empty max descendant
    level *)

val cousins_l1_l2_aux :
  config ->
  Geneweb_db.Driver.base ->
  string ->
  (* up l1 generations *)
  string ->
  (* down l2 generations *)
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int)
  list
  option

val cousins_implex_cnt :
  config ->
  Geneweb_db.Driver.base ->
  string ->
  (* up l1 generations *)
  string ->
  (* down l2 generations *)
  Geneweb_db.Driver.person ->
  int
(** for a list of "cousins" at level l1 l2, cousins_implex computes cousins
    already seen at levels l < l2. *)

val cousins_fold :
  (Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int)
  list ->
  (Geneweb_db.Driver.iper
  * (Geneweb_db.Driver.ifam list list * Geneweb_db.Driver.iper list * int)
  * int list)
  list
(** create a new list of (ip, (ifamll, iancl, cnt), lev) from list of (ip,
    ifaml, ianc, lev) The effect is to assemble multiple items under a single ip
*)

val anc_cnt_aux :
  config ->
  Geneweb_db.Driver.base ->
  int ->
  (* level *)
  bool ->
  (* up to ot at *)
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int)
  list
  option
(** Get the list of ancestors up to or at level *)

val desc_cnt_aux :
  config ->
  Geneweb_db.Driver.base ->
  int ->
  (* level *)
  bool ->
  (* up to ot at *)
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int)
  list
  option
(** Get the list of descendants up to or at level *)
