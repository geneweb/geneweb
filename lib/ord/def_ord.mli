type ('person, 'string) gen_pers_event = ('person, 'string) Def.gen_pers_event
type ('person, 'string) gen_fam_event = ('person, 'string) Def.gen_fam_event
type 'string gen_title = 'string Def.gen_title

val compare_gen_title :
  ('string -> 'string -> int) ->
  'string Def.gen_title ->
  'string Def.gen_title ->
  int

val compare_gen_pers_event :
  ('person -> 'person -> int) ->
  ('string -> 'string -> int) ->
  ('person, 'string) gen_pers_event ->
  ('person, 'string) gen_pers_event ->
  int

val compare_gen_fam_event :
  ('person -> 'person -> int) ->
  ('string -> 'string -> int) ->
  ('person, 'string) gen_fam_event ->
  ('person, 'string) gen_fam_event ->
  int
