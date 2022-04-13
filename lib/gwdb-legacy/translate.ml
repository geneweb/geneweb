

(* TODO modify according to changes to def *)

external as_def_person :
  ('iper, 'person, 'string) Dbdisk.gen_person ->
  ('iper, 'person, 'string) Def.gen_person = "%identity"

                                           
external as_dsk_person :
  ('iper, 'person, 'string) Def.gen_person ->
  ('iper, 'person, 'string) Dbdisk.gen_person = "%identity"


