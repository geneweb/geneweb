(** Default number of relatives that could be listed at the same page *)
val default_max_cnt : int

(** Retruns list of children of the giving family *)
val children_of_fam : Gwdb.base -> Gwdb.ifam -> Gwdb.iper list

(** Returns list of person's siblings that includes also half-blood siblings. Every sibling
    is annotated with parent's id and parent's sex. For common father's and mother's
    children father's annotation is preserved. *)
val siblings :
  Config.config ->
  Gwdb.base -> Gwdb.iper -> (Gwdb.iper * (Gwdb.iper * Def.sex)) list

(** [has_desc_lev conf base lev p] tells if person [p] has descendants at the level [lev].
    [lev] 2 represents his children, 3 represents grandchildren, etc. *)
val has_desc_lev :
  Config.config -> Gwdb.base -> int -> Gwdb.person -> bool

(** Tells if two family branches don't itersect *)
val br_inter_is_empty : ('a * 'b) list -> ('a * 'c) list -> bool

(** Same as [has_desc_lev] but used for a person's sibling as returned by [siblings]. *)
val sibling_has_desc_lev :
  Config.config -> Gwdb.base -> int -> Gwdb.iper * 'a -> bool

(** Returns sosa number calculated from the giving ancestors list. *)
val sosa_of_persons :
  Config.config -> Gwdb.base -> Gwdb.iper list -> int
