(** Terms to split: a place term ending with a frequent term, the comma
    forgotten, such as ["Montréal Canada"] beside ["Canada"].

    A term is frequent when its cluster of spellings ({!PlaceTerms.cluster_key})
    has at least [min_uses] uses. A term [t] is a candidate when it is made of
    two parts separated by a space, [left] and [right], [right] having the key
    of a frequent term, and [t] is not frequent itself. The split writes [left]
    and the most used spelling of the frequent term: ["Montréal, Canada"].

    Some names end with a frequent term and must stay as they are
    (["Nouvelle Écosse"], ["New York"]). A proposal is left out when the last
    word of [left] is a particle (["Saint-Pierre de Montréal"]); it is only
    checked by default when [left] is itself a term of the base (["Montréal"]).
*)

type proposal = {
  entry : PlaceTerms.entry;  (** the term to split *)
  left : string;  (** its first part *)
  right : string;  (** the most used spelling of the frequent term *)
  known : bool;  (** [left] is a term of the base *)
  occurrences : PlaceRename.occurrence list;
      (** where it is split: the contexts where the frequent term is not already
          a parent, the suburbs left out *)
}

val default_min_uses : int

val proposals : ?min_uses:int -> PlaceTerms.entry list -> proposal list
(** The proposals, grouped by frequent term (most used first), each group by
    decreasing uses. *)

val split_at :
  (string, string) Hashtbl.t -> string -> (string * string * string) option
(** [split_at frequent t]: [Some (left, right_text, key)] for the longest end of
    [t] after a space whose cluster key is in [frequent] (key -> spelling),
    [left] trimmed, without the separators at its end (["Vallières – Metz"]
    gives ["Vallières"]), and not ending with a particle. Exposed for the tests.
*)

val new_term : proposal -> string
(** ["left, right"] *)

val plan :
  Config.config ->
  Geneweb_db.Driver.base ->
  proposal list ->
  PlaceRename.item list
(** The renaming of all the proposals at once. A place where several terms are
    split is planned for the first one only: applying again does the others. *)
