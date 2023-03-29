

type iper = int
type ifam = int
type istr = int

let id x = x
let iper, ifam, istr = id, id, id

module Gwdb_utils = struct
  type iper = Gwdb.iper
  type ifam = Gwdb.ifam
  type istr = Gwdb.istr
  let iper = Obj.magic
  let ifam = Obj.magic
  let istr = Obj.magic
end


type person = (iper, iper, istr) Def.gen_person
type family = (iper, ifam, istr) Def.gen_family
type descend = int Def.gen_descend
type ascend = ifam Def.gen_ascend
type union = ifam Def.gen_union
type couple = iper Def.gen_couple



let default_strings () = [| ""; "?" |]
let empty_string = 0
let quest_string = 1

let union ~families = Def.({ family = families })
let couple ~father ~mother = Adef.couple father mother
let ascend ~parents = Def.({ Gwdb.no_ascend with parents })
let descend ~children = Def.({ children })

let person i = {
  (Mutil.empty_person
     empty_string
     quest_string)
  with occ = i; key_index = i
}

let family i = {
  (Mutil.empty_family empty_string)
  with fam_index = i
}


