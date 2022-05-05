

(* TODO modify according to changes to def *)

external as_def_person :
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person ->
  ('iper, 'person, 'string) Def.gen_person = "%identity"

                                           
external as_dsk_person :
  ('iper, 'person, 'string) Def.gen_person ->
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person = "%identity"


