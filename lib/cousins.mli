open Config

type one_cousin =
  Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int
(** Represents a cousin relationship:
    - cousin's iper
    - list of families linking cousin to common ancestor
    - common ancestor's iper
    - genealogical distance level *)

type cousins_i_j = one_cousin list
(** List of cousins at a specific (l1, l2) degree *)

type cousins_sparse
(** Sparse structure storing cousins at each ancestor/descendant level *)

val cousins_table : cousins_i_j array array

val default_max_cnt : int
(** Default number of relatives that could be listed at the same page *)

val max_cousin_level : config -> int
(** Maximum cousin level from config, defaults to 6 *)

val max_ancestor_level :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int -> int
(** Computes max ancestor level for a person. Priority: URL param > gwf config >
    fallback. Returns actual computed level capped by configured limit. *)

val max_descendant_level :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int -> int
(** Computes max descendant level for a person. Priority: URL param > gwf config
    > fallback. Returns actual computed level capped by limit. *)

val children_of_fam :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.iper list
(** Returns list of children of the given family *)

val siblings :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  (Geneweb_db.Driver.iper * (Geneweb_db.Driver.iper * Def.sex)) list
(** Returns list of person's siblings including half-siblings. Each sibling is
    annotated with parent's id and sex. For full siblings, father's annotation
    is preserved. *)

val has_desc_lev :
  config -> Geneweb_db.Driver.base -> int -> Geneweb_db.Driver.person -> bool
(** Tells if person has descendants at given level. Level 1 = children, level 2
    = grandchildren, etc. *)

val br_inter_is_empty : ('a * 'b) list -> ('a * 'c) list -> bool
(** Tells if two family branches don't intersect *)

val sibling_has_desc_lev :
  config -> Geneweb_db.Driver.base -> int -> Geneweb_db.Driver.iper * 'a -> bool
(** Same as [has_desc_lev] but for a sibling as returned by [siblings]. *)

val init_cousins_cnt :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> cousins_sparse
(** Builds sparse structure of cousins at each (l1, l2) level. For each cousin,
    records their families, common ancestor, and genealogical distance, along
    with birth/death date ranges. Supports optional disk caching. *)

val min_max_date :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  string ->
  string ->
  int option
(** Returns min or max birth/death year for cousins at level (l1, l2). The bool
    parameter selects min (true) or max (false). *)

val max_l1_l2 :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  int * int
(** Returns (max_ancestor_level, max_descendant_level) where data exists in the
    sparse structure. *)

val cousins_l1_l2_aux :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  string ->
  string ->
  Geneweb_db.Driver.person ->
  one_cousin list option
(** Returns list of cousins at specified (l1, l2) level where l1 = generations
    up to common ancestor, l2 = generations down from ancestor. Examples: (0,0)
    = self, (1,1) = siblings, (2,1) = uncles/aunts, (2,2) = cousins. *)

val cousins_implex_cnt :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  string ->
  string ->
  Geneweb_db.Driver.person ->
  int
(** Counts cousins at level (l1, l2) already seen at levels l < l2, used to
    detect implex (multiple relationship paths). *)

val cousins_fold :
  one_cousin list ->
  (Geneweb_db.Driver.iper
  * (Geneweb_db.Driver.ifam list list * Geneweb_db.Driver.iper list * int)
  * int list)
  list
(** Groups one_cousin entries by iper, assembling multiple relationship paths
    under a single person. Returns (iper, (families, ancestors, path_count),
    levels). *)

val anc_cnt_aux :
  config ->
  Geneweb_db.Driver.base ->
  int ->
  bool ->
  Geneweb_db.Driver.person ->
  one_cousin list option
(** Returns ancestors at level (if bool=true) or up to level (if bool=false).
    Level 1 = parents, level 2 = grandparents, etc. *)

val desc_cnt_aux :
  config ->
  Geneweb_db.Driver.base ->
  int ->
  bool ->
  Geneweb_db.Driver.person ->
  one_cousin list option
(** Returns descendants at level (if bool=true) or up to level (if bool=false).
    Level 1 = children, level 2 = grandchildren, etc. *)
