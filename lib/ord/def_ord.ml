type cdate = Adef.cdate

let compare_cdate cd1 cd2 =
  let od1 = Date.od_of_cdate cd1 in
  let od2 = Date.od_of_cdate cd2 in
  match (od1, od2) with
  | Some d1, Some d2 -> Date.compare_date d1 d2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

type witness_kind = [%import: Def.witness_kind] [@@deriving ord]

type 'string gen_pers_event_name = [%import: 'string Def.gen_pers_event_name]
[@@deriving ord]

type ('person, 'string) gen_pers_event =
  [%import: ('person, 'string) Def.gen_pers_event]
[@@deriving ord]

type 'string gen_fam_event_name = [%import: 'string Def.gen_fam_event_name]
[@@deriving ord]

type ('person, 'string) gen_fam_event =
  [%import: ('person, 'string) Def.gen_fam_event]
[@@deriving ord]

type 'string gen_title_name = [%import: 'string Def.gen_title_name]
[@@deriving ord]

type 'string gen_title = [%import: 'string Def.gen_title] [@@deriving ord]
