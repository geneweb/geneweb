
val diff_person : base:Gwdb.base ->
  previously:(Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  now:(Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  Diff_types.person_diff
