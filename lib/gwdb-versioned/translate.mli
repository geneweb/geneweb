val legacy_to_def_person :
  'string ->
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person ->
  ('iper, 'person, 'string) Def.gen_person

val as_legacy_person :
  ('iper, 'person, 'string) Def.gen_person ->
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person

val legacy_to_def_pevent :
  'string ->
  ('iper, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_pers_event ->
  ('iper, 'string) Def.gen_pers_event

val as_legacy_pevent :
  ('iper, 'string) Def.gen_pers_event ->
  ('iper, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_pers_event

val legacy_to_def_family :
  'string ->
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_family ->
  ('iper, 'person, 'string) Def.gen_family

val as_legacy_family :
  ('iper, 'person, 'string) Def.gen_family ->
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_family

val legacy_to_def_fevent :
  'string ->
  ('iper, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_fam_event ->
  ('iper, 'string) Def.gen_fam_event

val as_legacy_fevent :
  ('iper, 'string) Def.gen_fam_event ->
  ('iper, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_fam_event
