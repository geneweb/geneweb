type date = [%import: Def.date] [@@deriving show { with_path = false }]
and calendar = [%import: Def.calendar] [@@deriving show { with_path = false }]
and dmy = [%import: Def.dmy] [@@deriving show { with_path = false }]
and dmy2 = [%import: Def.dmy2] [@@deriving show { with_path = false }]
and precision = [%import: Def.precision] [@@deriving show { with_path = false }]

type cdate = Adef.cdate
let pp_cdate fmt x =
  match Adef.od_of_cdate x with
  | Some d -> pp_date fmt d
  | None -> Format.fprintf fmt "None"
let show_cdate x =
  match Adef.od_of_cdate x with
  | Some d -> show_date d
  | None -> "None"

type relation_kind = [%import: Def.relation_kind] [@@deriving show { with_path = false }]

type divorce = [%import: Def.divorce] [@@deriving show { with_path = false }]

type death_reason = [%import: Def.death_reason] [@@deriving show { with_path = false }]

type death = [%import: Def.death] [@@deriving show { with_path = false }]

type burial = [%import: Def.burial] [@@deriving show { with_path = false }]

type access = [%import: Def.access] [@@deriving show { with_path = false }]

type 'string gen_title_name = [%import: 'string Def.gen_title_name] [@@deriving show { with_path = false }]

type 'string gen_title = [%import: 'string Def.gen_title] [@@deriving show { with_path = false }]

type witness_kind = [%import: Def.witness_kind] [@@deriving show { with_path = false }]

type 'string gen_pers_event_name = [%import: 'string Def.gen_pers_event_name] [@@deriving show { with_path = false }]

type ('person, 'string) gen_pers_event = [%import: ('person, 'string) Def.gen_pers_event] [@@deriving show { with_path = false }]

type 'string gen_fam_event_name = [%import: 'string Def.gen_fam_event_name] [@@deriving show { with_path = false }]

type ('person, 'string) gen_fam_event = [%import: ('person, 'string) Def.gen_fam_event] [@@deriving show { with_path = false }]

type relation_type = [%import: Def.relation_type] [@@deriving show { with_path = false }]

type ('person, 'string) gen_relation = [%import: ('person, 'string) Def.gen_relation] [@@deriving show { with_path = false }]

type sex = [%import: Def.sex] [@@deriving show { with_path = false }]

type place = [%import: Def.place] [@@deriving show { with_path = false }]

type ('iper, 'person, 'string) gen_person = [%import: ('iper, 'person, 'string) Def.gen_person] [@@deriving show { with_path = false }]

type fix = Adef.fix
let pp_fix fmt x = Format.fprintf fmt "%d" @@ Adef.fix_repr x
let show_fix x = string_of_int @@ Adef.fix_repr x

type 'family gen_ascend
  = 'family Def.gen_ascend
  = { parents : 'family option ; consang : fix }
 [@@deriving show { with_path = false }]

type 'family gen_union = [%import: 'family Def.gen_union] [@@deriving show { with_path = false }]

type ('person, 'ifam, 'string) gen_family = [%import: ('person, 'ifam, 'string) Def.gen_family] [@@deriving show { with_path = false }]

type 'person gen_couple = 'person Adef.gen_couple
[@polyprinter fun pp fmt x ->
  fprintf fmt "[ %a ; %a ]" pp (Adef.father x) pp (Adef.mother x)
] [@@deriving show { with_path = false }]

type 'person gen_descend = [%import: 'person Def.gen_descend ] [@@deriving show { with_path = false }]

let show_pau base p =
  let a = Gwdb.gen_ascend_of_person p in
  let u = Gwdb.gen_union_of_person p in
  let p = Gwdb.gen_person_of_person p in
  let p = Futil.map_person_ps Gwdb.string_of_iper (Gwdb.sou base) p in
  let p = { p with key_index = Gwdb.string_of_iper p.key_index } in
  let a = { a with parents =
                     match a.parents with
                     | Some ifam -> Some (Gwdb.string_of_ifam ifam)
                     | None -> None
          } in
  let u = { Def.family = Array.map Gwdb.string_of_ifam u.family } in
  ( [%show: (string,string,string) gen_person] p
  , [%show: string gen_ascend] a
  , [%show: string gen_union] u
  )

let show_fcd base f =
  let c = Gwdb.gen_couple_of_family f in
  let d = Gwdb.gen_descend_of_family f in
  let f = Gwdb.gen_family_of_family f in
  let f = Futil.map_family_ps Gwdb.string_of_iper Gwdb.string_of_ifam (Gwdb.sou base) f in
  let c = Futil.map_couple_p false Gwdb.string_of_iper c in
  let d = Futil.map_descend_p Gwdb.string_of_iper d  in
  ( [%show: (string,string,string) gen_family] f
  , [%show: string gen_couple] c
  , [%show: string gen_descend] d
  )
