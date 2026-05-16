open Config

module CoordMap : Map.S with type key = int * int
(** Coordinate map keyed by [(l1, l2)] cell, where [l1] is ancestor depth and
    [l2] descendant depth from the common ancestor. Backs the two fields of
    [cousins_sparse] below. *)

type one_cousin =
  Geneweb_db.Driver.iper
  * Geneweb_db.Driver.ifam list
  * Geneweb_db.Driver.iper
  * int
(** Raw cousin record produced when expanding [init_cousins_cnt]. Fields:
    - [iper] : the cousin's person id
    - [ifam list] : family chain from the cousin up to the common ancestor,
      innermost first; last entry is the ancestor's parental family
    - [iper] : the common ancestor's person id
    - [int] : genealogical distance level (used by [cousins_fold]) *)

type cousins_i_j = one_cousin list
(** All cousin records at one [(l1, l2)] cell of the matrix, where [l1] is the
    ancestor depth and [l2] the descendant depth from that ancestor. *)

type cousins_sparse = {
  data : one_cousin list CoordMap.t;
  dates : (int * int) CoordMap.t;
  max_i : int;
  max_j : int;
}
(** Sparse [(l1, l2) -> cousins_i_j] map paired with a sibling
    [(l1, l2) -> (min_year, max_year)] map for per-cell date spans, plus cached
    [max_i] / [max_j] populated bounds. Both maps share their key set; an absent
    key denotes an empty cell. Built once per request by [init_cousins_cnt];
    consumed read-only by every other function below and by the JSON serializers
    in [Cousins_display]. *)

val max_cousin_level : config -> int
(** [max_cousins_level] from the [.gwf], defaults to 6. Clamps the [v1] and [v2]
    URL parameters in the classic HTML renderer. *)

val max_ancestor_level :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int -> int
(** [max_ancestor_level conf base ip max_lvl] returns the actual height of the
    ascending tree rooted at [ip], capped from above. The cap is [max_lvl] when
    positive, else [max_anc_level] from the [.gwf], else 12. Returns 0 if [ip]
    has no parents. *)

val max_descendant_level :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int -> int
(** Symmetric to [max_ancestor_level] for descent. Cap is [max_lvl] when
    positive, else [max_desc_level] from the [.gwf], else 16. *)

val children_of_fam :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.iper list
(** Children of one family as a list, no filtering. *)

val siblings :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  (Geneweb_db.Driver.iper * (Geneweb_db.Driver.iper * Def.sex)) list
(** Full and half siblings of [ip]. Each entry is paired with the parent through
    whom the link runs: [(parent_iper, parent_sex)]. Full siblings appear once
    with the father's annotation; half-siblings appear once per shared parent.
    Empty when [ip] has no parents. *)

val has_desc_lev :
  config -> Geneweb_db.Driver.base -> int -> Geneweb_db.Driver.person -> bool
(** [has_desc_lev conf base lev p] tests whether [p] has at least one descendant
    exactly at generation [lev] (1 = children, 2 = grandchildren, …).
    Short-circuits on the first match. *)

val sibling_has_desc_lev :
  config -> Geneweb_db.Driver.base -> int -> Geneweb_db.Driver.iper * 'a -> bool
(** [has_desc_lev] curried over a sibling pair [(ip, _)] as produced by
    [siblings]. *)

val init_cousins_cnt :
  config ->
  Geneweb_db.Driver.base ->
  ?up_to:int ->
  Geneweb_db.Driver.person ->
  cousins_sparse
(** Builds the full sparse cousin matrix for person [p] up to some ancestor
    depth. The ceiling, in priority order:
    - [up_to] when provided — explicit, no base-wide ancestor scan (used by the
      incremental JSON endpoint)
    - [v] URL parameter when present and positive — effective ceiling is [v + 1]
    - [max_ancestor_level] computed on [p] otherwise

    Uses and populates the per-level disk cache when [cache_cousins_tool=yes] in
    the [.gwf]; cache files live in [caches/cousins_json/<key>_level_N.json]
    where [<key>] is [strip_lower(surname).occ.strip_lower(first_name)]. *)

val extract_level : cousins_sparse -> int -> cousins_sparse
(** [extract_level sparse level] returns a sub-sparse keeping only the cells of
    the form [(level, j)] of [sparse], with their date entries copied over. The
    result's [max_i] is set to [level]; its [max_j] is the largest [j] with a
    non-empty cell at that level. Used by [Cousins_display] to feed
    [cousins_level_to_json] with a single-level slice rather than the full
    sparse plus a [level] argument. *)

val max_l1_l2 :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  int * int
(** [(max_anc, max_desc)] over populated cells of [sparse]: [max_anc] is the
    largest [l1] with a non-empty [(l1, 0)] cell; [max_desc] is the largest
    [j - i] across all populated cells. Legacy template-binding shape: [conf],
    [base] and [person] are ignored. *)

val min_max_date :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  [ `Min | `Max ] ->
  string ->
  string ->
  int option
(** [min_max_date sparse _ _ _ pick l1 l2] returns the min year ([`Min]) or the
    max year ([`Max]) aggregated over cell
    [(int_of_string l1, int_of_string l2)]. Returns [None] when [l1] or [l2] is
    not a valid integer, or when the cell has no dated cousin. Legacy
    template-binding shape: [conf], [base] and [person] are accepted for
    signature uniformity and ignored. *)

val cousins_l1_l2_aux :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  string ->
  string ->
  Geneweb_db.Driver.person ->
  one_cousin list option
(** [cousins_l1_l2_aux sparse _ _ l1 l2 _] returns the raw cousin list at cell
    [(int_of_string l1, int_of_string l2)]. Reference points:
    - [(0, 0)] : self
    - [(1, 1)] : siblings
    - [(2, 1)] : aunts / uncles
    - [(2, 2)] : first cousins

    Raises [Failure] on non-integer arguments. Legacy template-binding shape. *)

val cousins_implex_cnt :
  cousins_sparse ->
  config ->
  Geneweb_db.Driver.base ->
  string ->
  string ->
  Geneweb_db.Driver.person ->
  int
(** Number of cousin records in cell [(l1, l2)] whose [iper] also appears in
    some shallower cell [(l1, l2')] with [l2' < l2]. Semantically relevant for
    the ancestor column [(i, 0)] (implex / multi-path ascendancy); for other
    cells the value is computed identically but its interpretation is less
    clear. Legacy template-binding shape. *)

val cousins_fold :
  one_cousin list ->
  (Geneweb_db.Driver.iper
  * (Geneweb_db.Driver.ifam list list * Geneweb_db.Driver.iper list * int)
  * int list)
  list
(** Groups raw [one_cousin] records by [iper], aggregating multiple relationship
    paths leading to the same cousin:
    - [iper] : the cousin's id
    - [ifam list list] : the family chains, one inner list per distinct path
    - [iper list] : distinct common ancestors reached
    - [int] : path count
    - [int list] : per-path levels

    Used internally by [cousins_implex_cnt] and historically by template
    [foreach;cousin;] iteration. *)

val init_asc_cnt :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  one_cousin list array
(** Pre-computes the per-level ancestor array for [p]. Index 0 holds [p]
    himself, index 1 his parents, etc., up to
    [max_ancestor_level conf base (get_iper p) 0]. Each entry retains every path
    reaching that level (duplicates preserved so implex counts remain accurate).
    Intended to be passed as [?asc_cnt] of [anc_cnt_aux] when calling it several
    times on the same person. *)

val anc_cnt_aux :
  ?asc_cnt:one_cousin list array ->
  config ->
  Geneweb_db.Driver.base ->
  [ `At_level of int | `Up_to of int ] ->
  Geneweb_db.Driver.person ->
  one_cousin list option
(** [anc_cnt_aux ?asc_cnt conf base mode p] returns ancestors of [p]:
    - exactly at level [n] when [mode] is [`At_level n]
    - the union of levels [1 .. n] when [mode] is [`Up_to n]

    Returns [None] when the requested level exceeds the pre-computed depth.
    @param asc_cnt
      reuse a pre-computed array from [init_asc_cnt] across several calls on the
      same person; when omitted it is rebuilt internally. *)

val desc_cnt_aux :
  config ->
  Geneweb_db.Driver.base ->
  [ `At_level of int | `Up_to of int ] ->
  Geneweb_db.Driver.person ->
  one_cousin list option
(** Symmetric to [anc_cnt_aux] for descent. With [`Up_to n] the result is
    additionally deduplicated by [iper] (latest path wins) — a property that
    [anc_cnt_aux] does not enforce. *)
