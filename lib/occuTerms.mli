(** Analysis of the occupation strings, to group the spellings of an occupation.

    An occupation string is a list of occupations separated by commas:
    ["laboureur, marchand"]. A comma between parentheses or brackets does not
    separate two occupations: ["marchand (de vin, de grains)"] is one. Every
    occupation keeps its span in the raw string, so it can be rewritten in
    place; the spaces after a comma are not part of it, the spaces before the
    next comma are (["laboureur "] is a spelling of its own, to be merged with
    ["laboureur"]).

    The occupations are then grouped in clusters of spellings with
    {!PlaceTerms.cluster_key}: case, accents, hyphens, spaces and invisible
    characters are ignored, so ["Laboureur"], ["laboureur"] and ["laboureur "]
    are in the same cluster. *)

type span = SourceTerms.span = { text : string; pos : int; len : int }

val spans : string -> span list
(** The occupations of a raw occupation string, in order. *)

val key : string -> string
(** {!PlaceTerms.cluster_key} *)

type cluster = {
  ckey : string;
  spellings : SourceTerms.spelling list;  (** most used first *)
  total : int;  (** uses of the cluster *)
}

type data = (Geneweb_db.Driver.istr * string * int) list
(** The occupation strings used, with the number of persons using them. *)

val load : Geneweb_db.Driver.base -> data
(** One pass over the persons, regardless of access rights (as m=MOD_DATA). *)

val clusters : data -> cluster list
(** The clusters of two spellings or more, and the single spellings that are not
    {!PlaceTerms.is_clean}, by decreasing uses. *)

val samples :
  data -> string list -> int -> (string * (string * span list) list) list
(** [samples d spellings n]: for each spelling, up to [n] strings containing it,
    with its spans. *)

val plan :
  Geneweb_db.Driver.base ->
  data ->
  terms:string list ->
  new_term:string ->
  (PlaceRename.item * span list) list
(** Replacing the spellings [terms] by [new_term] in every occupation string,
    with the replaced spans of each old string. *)
