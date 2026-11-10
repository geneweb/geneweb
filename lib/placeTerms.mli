(** Dictionary of individual place terms.

    A full place such as ["[Boileau] - Saint-Martin, Manche, France"] is made of
    several terms (suburb, town, department, country...). This module explodes
    every distinct place string of the base into its terms, so that each term
    can be listed once with every context (chain of parent terms) and every full
    place it occurs in. This tells whether a term can be corrected globally or
    must be fixed context by context or place by place. *)

type place = {
  istr : Geneweb_db.Driver.istr;
  raw : string;  (** the full place as stored *)
  uses : int;  (** number of persons and families referring to it *)
}

type context = {
  parents : string list;  (** ancestor terms, root first; [[]] at top level *)
  suburb : bool;  (** the term is the bracketed suburb of these places *)
  places : place list;  (** full places, sorted *)
  c_uses : int;  (** sum of [uses] over [places] *)
}

type entry = {
  term : string;  (** exact spelling *)
  key : string;  (** [Name.lower term], used to search by prefix *)
  cluster : string;
      (** {!cluster_key}[ term]: entries sharing it are spellings of the same
          term *)
  contexts : context list;  (** sorted root first *)
  nb_places : int;
  uses : int;
}

val place_uses : Geneweb_db.Driver.base -> (Geneweb_db.Driver.istr * int) list
(** Every non-empty place string used by an event (main fields and person/family
    events), with the number of persons and families using it, read from the
    index of the dictionaries. *)

val build :
  ?only:(string -> bool) ->
  Config.config ->
  Geneweb_db.Driver.base ->
  entry list
(** All terms of the base, sorted alphabetically, or those satisfying [only]
    (with their contexts and places, sorted too: costly for every term of a
    large base). *)

val build_summary : Config.config -> Geneweb_db.Driver.base -> entry list
(** All terms of the base with their numbers of places and uses, without
    contexts ([contexts = []]) nor sorting nor [key] ([key = ""]): enough for
    {!clusters}. *)

val in_cluster : string -> string -> bool
(** [in_cluster key] tells whether a term belongs to the cluster [key]
    ({!cluster_key}), memoized, for the [only] of {!build}. *)

val of_places :
  ?only:(string -> bool) ->
  bool ->
  (Geneweb_db.Driver.istr * string * int) list ->
  entry list
(** [of_places inverted places] is the pure core of {!build}: it explodes the
    given [(istr, raw place, uses)] triples into sorted entries. *)

val summary : bool -> (Geneweb_db.Driver.istr * string * int) list -> entry list
(** The pure core of {!build_summary}: the terms with their numbers only, what
    the list of the clusters needs. *)

val clean_term : string -> string
(** A term as written by a merging of spellings: [Util.only_printable] (as the
    check_data fix of invisible characters), and no space or non-breaking space
    at both ends. *)

val is_clean : string -> bool
(** [is_clean s] is [clean_term s = s]: no invisible character, no space or
    non-breaking space at both ends. *)

val key_text : string -> string
(** [clean_term], the non-breaking spaces inside replaced by spaces: the text
    the cluster keys are computed from. *)

val cluster_key : string -> string
(** [cluster_key t] is [Name.abbrev (Name.lower t)]: case, accents, hyphens and
    spaces are ignored, and "Saint"/"Sainte" are read as "St"/"Ste", so
    ["Saint-Malo"], ["St Malo"], ["Saint Malo"] and ["saint-malo"] share the key
    ["st malo"]. *)

val clusters : entry list -> (string * entry list) list
(** The clusters of spellings: entries grouped by [cluster], keeping the groups
    of two terms or more, and the single terms that are not {!is_clean} (to
    remove their invisible characters and spaces). Each group is sorted by
    decreasing uses, the groups by decreasing total uses. *)
