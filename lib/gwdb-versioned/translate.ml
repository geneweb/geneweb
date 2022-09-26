

(* TODO modify according to changes to def *)

external legacy_to_def_person :
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person ->
  ('iper, 'person, 'string) Def.gen_person = "%identity"

                                           
external as_legacy_person :
  ('iper, 'person, 'string) Def.gen_person ->
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person = "%identity"



external legacy_to_def_pers_event :
  ('iper, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_pers_event ->
  ('iper, 'string) Def.gen_pers_event = "%identity"

external as_legacy_pers_event :
  ('iper, 'string) Def.gen_pers_event ->
  ('iper, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_pers_event = "%identity"
