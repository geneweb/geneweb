(** Places to complete: a place whose terms are the beginning of the terms of a
    more complete place, e.g. ["Granville"], ["Granville, Manche"] and
    ["Granville (Manche)"] beside ["Granville, Manche, France"].

    The terms are compared leaf first, as written: a place is in the group of a
    more complete place when its terms are a prefix of the terms of that place,
    with the same suburb (["aaaa (bbbb)"] and ["aaaa, bbbb"] have the same
    terms). A place that is not a prefix of any other place is the target of its
    group; places with the same terms as the target are variants of it (another
    way of writing the same place). A short place may be in several groups
    (["Saint-Martin"] beside ["Saint-Martin, Manche"] and
    ["Saint-Martin, Calvados"]): it is then ambiguous. *)

type group = {
  targets : PlaceTerms.place list;
      (** the most complete place and its variants, most used first *)
  members : PlaceTerms.place list;  (** the less complete places, sorted *)
  uses : int;  (** total uses of the group *)
}

val key : group -> Geneweb_db.Driver.istr
(** The id of the first target, which identifies the group. *)

val of_places :
  bool -> (Geneweb_db.Driver.istr * string * int) list -> group list
(** [of_places inverted places]: the groups of two places or more, by decreasing
    uses. *)

val build : Config.config -> Geneweb_db.Driver.base -> group list
(** The groups of the places of the base. *)

val find : group list -> Geneweb_db.Driver.istr -> group option
(** The group of key [istr]. *)

val count_groups : group list -> Geneweb_db.Driver.istr -> int
(** In how many groups a place is a member: more than one means ambiguous. *)
