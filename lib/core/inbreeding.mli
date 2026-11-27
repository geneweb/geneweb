(** Inbreeding coefficient and implexes of a person, computed on its ancestry
    and kept in the index of consanguinities ({!Geneweb_db.ConsangIndex}).

    A value is read from the index when it holds one, and the coefficient
    otherwise from the base, as [consang] left it. What is missing or outdated
    is computed on the graph of the ancestors, read once from the base, and
    stored for the next readers: the kinship of the parents is obtained with the
    algorithm of {!Consang}, in the order of this graph instead of the
    topological sort of the whole base, and the coefficients of the common
    ancestors it relies on are obtained the same way, the oldest first.

    The index is outdated by the commits of the base that change the ancestry of
    a person: the values never lag behind the data. *)

type t = {
  coefficient : float option;
      (** Kinship of the parents; [None] when it was given up, see {!budget}. *)
  implexes : int;
      (** Ancestors reached through several children by a walk of the ancestry
          expanding each ancestor once and ignoring the persons named "? ?",
          like {!Implex.structure}. *)
}

val budget : int
(** Ancestors that one call may weigh, all its computations added up. Beyond it
    the call gives up: on a large ancestry whose coefficients were never
    computed, the work is the one of [consang]. The coefficients obtained are
    stored, and the person asked for is marked so that the next calls do not try
    again before [consang] ran or its ancestry changed. *)

val is_unknown : Geneweb_db.Driver.person -> bool
(** Tells whether the first name and the surname of the person are "?". *)

val stored : Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> float option
(** [stored base ip] is the coefficient of [ip] held by the index or by the
    base, [None] when missing or outdated: nothing is computed. *)

val of_person : Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> t
(** [of_person base ip] is one read of the index when it holds both values;
    otherwise the missing ones are computed and stored. *)

val of_ancestry :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  ((Geneweb_db.Driver.iper -> float option) -> 'a) ->
  'a
(** [of_ancestry base ip k] calls [k] with the coefficients of [ip] and of its
    ancestors, computed on the graph of the ancestry of [ip] read once and
    stored when [k] returns; [None] for any other person and beyond the
    {!budget}. *)
