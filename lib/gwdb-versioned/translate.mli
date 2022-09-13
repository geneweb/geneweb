

val legacy_to_def_person : ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person ->
                 ('iper, 'person, 'string) Def.gen_person

val as_legacy_person : ('iper, 'person, 'string) Def.gen_person ->
                 ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person

