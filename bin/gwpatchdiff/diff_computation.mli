
val diff_person : base:Gwdb.base ->
  iper:Gwdb.iper ->
  previously:(Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  now:(Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  Diff_types.Person_diff.t

val updates_from_patch : Gwdb.base -> Diff_types.Person_diff.t list
