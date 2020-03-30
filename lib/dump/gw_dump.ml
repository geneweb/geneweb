type fix = Adef.fix
let pp_fix fmt x = Format.fprintf fmt "%f" @@ Adef.float_of_fix x
let show_fix x = string_of_float @@ Adef.float_of_fix x

type date = [%import: Def.date] [@@deriving show]
and calendar = [%import: Def.calendar] [@@deriving show]
and dmy = [%import: Def.dmy] [@@deriving show]
and dmy2 = [%import: Def.dmy2] [@@deriving show]
and precision = [%import: Def.precision] [@@deriving show]

type cdate = Adef.cdate
let pp_cdate fmt x = pp_date fmt @@ Adef.date_of_cdate x
let show_cdate x = show_date @@ Adef.date_of_cdate x

type f_relation_kind = [%import: Def.f_relation_kind] [@@deriving show]

type relation_kind = [%import: Def.relation_kind] [@@deriving show]

type divorce = [%import: Def.divorce] [@@deriving show]

type death_reason = [%import: Def.death_reason] [@@deriving show]

type death = [%import: Def.death] [@@deriving show]

type burial = [%import: Def.burial] [@@deriving show]

type access = [%import: Def.access] [@@deriving show]

type 'string gen_title_name = [%import: 'string Def.gen_title_name] [@@deriving show]

type 'string gen_title = [%import: 'string Def.gen_title] [@@deriving show]

type witness_kind = [%import: Def.witness_kind] [@@deriving show]

type 'string gen_pers_event_name = [%import: 'string Def.gen_pers_event_name] [@@deriving show]

type ('person, 'string) gen_pers_event = [%import: ('person, 'string) Def.gen_pers_event] [@@deriving show]

type 'string gen_fam_event_name = [%import: 'string Def.gen_fam_event_name] [@@deriving show]

type ('person, 'string) gen_fam_event = [%import: ('person, 'string) Def.gen_fam_event] [@@deriving show]

type relation_type = [%import: Def.relation_type] [@@deriving show]

type ('person, 'string) gen_relation = [%import: ('person, 'string) Def.gen_relation] [@@deriving show]

type sex = [%import: Def.sex] [@@deriving show]

type place = [%import: Def.place] [@@deriving show]

type ('iper, 'person, 'string) gen_person = [%import: ('iper, 'person, 'string) Def.gen_person] [@@deriving show]

type 'family gen_ascend = [%import: 'family Def.gen_ascend] [@@deriving show]

type 'family gen_union = [%import: 'family Def.gen_union] [@@deriving show]

type ('person, 'ifam, 'string) gen_family = [%import: ('person, 'ifam, 'string) Def.gen_family] [@@deriving show]

type 'person gen_couple = 'person Adef.gen_couple
[@polyprinter fun pp fmt x ->
  fprintf fmt "[ %a ; %a ]" pp (Adef.father x) pp (Adef.mother x)
]

type 'person gen_descend = [%import: 'person Def.gen_descend ] [@@deriving show]
