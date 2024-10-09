include module type of Gwdb_driver

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
