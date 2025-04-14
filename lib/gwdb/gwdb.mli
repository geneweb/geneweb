include module type of Gwdb_driver

module IperSet : sig
  include Set.S with type elt = iper
end

module IfamSet : sig
  include Set.S with type elt = ifam
end

val insert_person :
  base ->
  (_, iper, istr) Def.gen_person ->
  ifam Def.gen_ascend ->
  ifam Def.gen_union ->
  iper

val insert_family :
  base ->
  (iper, ifam, istr) Def.gen_family ->
  iper Def.gen_couple ->
  iper Def.gen_descend ->
  ifam

val p_first_name : base -> person -> string
val p_surname : base -> person -> string
val person_misc_names : base -> person -> (person -> title list) -> string list

val nobtitles :
  base -> string list lazy_t -> string list lazy_t -> person -> title list

val children_of_p : base -> person -> iper list
val parents_of_person : base -> person -> iper Adef.gen_couple option

type person_reference = private {
  surname : string;
  first_name : string;
  occurrence_number : int;
}

val person_reference : base -> person -> person_reference
val search_indexes_can_be_initialized_on_the_fly : base -> bool
