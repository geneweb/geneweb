(** Terms to join: two consecutive terms of a place written as one frequent
    term, such as ["Paris, 1er"] or ["Paris, 16e"] beside ["Paris 1er"] and
    ["Paris 16e"]; and a term written with another numbering than a frequent
    term, such as ["Paris XIVe"] or ["Paris 14ème"] beside ["Paris 14e"].

    The terms are compared by {!key}: their cluster key
    ({!PlaceTerms.cluster_key}) with every ordinal number, in arabic or roman
    figures, written ["<n>e"]: ["1er"], ["1re"], ["Ier"] give ["1e"], ["XIVe"],
    ["14ème"], ["14"] give ["14e"]. A term of two words or more is frequent when
    the terms of its key have at least [min_uses] uses; the most used clean
    spelling is the one written. *)

val key : string -> string
(** ["Paris XIVe"] and ["Paris 14e"] give ["paris 14e"]. *)

val default_min_uses : int

type place = {
  istr : Geneweb_db.Driver.istr;
  raw : string;
  pos : int;  (** start of the replaced text in [raw] *)
  len : int;  (** its length *)
}

type proposal = {
  pkey : string;  (** identifies the proposal in a form *)
  written : string;  (** as written: ["Paris, 1er"], ["Paris XIVe"] *)
  target : string;  (** the frequent term: ["Paris 1er"], ["Paris 14e"] *)
  places : place list;
  uses : int;  (** uses of these places *)
}

val proposals :
  ?min_uses:int ->
  bool ->
  PlaceTerms.entry list ->
  (Geneweb_db.Driver.istr * string * int) list ->
  proposal list
(** [proposals ~min_uses inverted entries places]: [entries] are the terms of
    the [(istr, raw, uses)] places. Grouped by target (the most used first),
    each group by decreasing uses. In a place, two terms joined are not proposed
    again alone. *)

val plan : Geneweb_db.Driver.base -> proposal list -> PlaceRename.item list
(** Every place of the proposals with all its replacements. *)
