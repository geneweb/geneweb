type calendar = [%import: Adef.calendar] [@@deriving show { with_path = false }]
type dmy2 = [%import: Adef.dmy2] [@@deriving show { with_path = false }]

type date = [%import: Adef.date] [@@deriving show { with_path = false }]
and dmy = [%import: Adef.dmy] [@@deriving show { with_path = false }]

and precision = [%import: Adef.precision]
[@@deriving show { with_path = false }]

(* HACK: The type ['a gen_title] contains payloads of type [Adef.cdate]. So,
   the printer generated par the ppx show is looking for [Adef.pp_cdate]. *)
module Adef = struct
  include Adef

  let pp_cdate fmt x =
    match Date.od_of_cdate x with
    | Some d -> pp_date fmt d
    | None -> Format.fprintf fmt "None"
end

let pp_cdate = Adef.pp_cdate

type 'string gen_title_name = [%import: 'string Def.gen_title_name]
[@@deriving show { with_path = false }]

type 'string gen_title = [%import: 'string Def.gen_title]
[@@deriving show { with_path = false }]

type relation_type = [%import: Def.relation_type]
[@@deriving show { with_path = false }]

type ('person, 'string) gen_relation =
  [%import: ('person, 'string) Def.gen_relation]
[@@deriving show { with_path = false }]

type sex = [%import: Def.sex] [@@deriving show { with_path = false }]
type access = [%import: Def.access] [@@deriving show { with_path = false }]

type death_reason = [%import: Def.death_reason]
[@@deriving show { with_path = false }]

type death = [%import: Def.death] [@@deriving show { with_path = false }]
type burial = [%import: Def.burial] [@@deriving show { with_path = false }]

type witness_kind = [%import: Def.witness_kind]
[@@deriving show { with_path = false }]

type 'string gen_pers_event_name = [%import: 'string Def.gen_pers_event_name]
[@@deriving show { with_path = false }]

type ('person, 'string) gen_pers_event =
  [%import: ('person, 'string) Def.gen_pers_event]
[@@deriving show { with_path = false }]

type ('iper, 'person, 'string) gen_person =
  [%import: ('iper, 'person, 'string) Def.gen_person]
[@@deriving show { with_path = false }]

type relation_kind = [%import: Def.relation_kind]
[@@deriving show { with_path = false }]

type 'string gen_fam_event_name = [%import: 'string Def.gen_fam_event_name]
[@@deriving show { with_path = false }]

type ('person, 'string) gen_fam_event =
  [%import: ('person, 'string) Def.gen_fam_event]
[@@deriving show { with_path = false }]

let pp_fix fmt x = Format.fprintf fmt "%d" @@ Adef.fix_repr x
let show_fix x = string_of_int @@ Adef.fix_repr x

type divorce = [%import: Def.divorce] [@@deriving show { with_path = false }]
