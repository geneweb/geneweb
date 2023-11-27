open Diff_types

let make_diff previously now = {previously; now}

let diff cmp previously now = 
  if cmp previously now <> 0 then
    Some (make_diff (Some previously) (Some now))
  else None

let list_o = function
  | [] -> None
  | l -> Some l

let diff_list cmp previously now =
  let rec aux l1 l2 = match l1, l2 with
    | x :: xs, y :: ys when cmp x y = 0 -> aux xs ys
    | [], [] -> true
    | _ -> false
  in
  if aux previously now then None
  else Some (make_diff (list_o previously) (list_o now))

let _diff_string = diff String.compare
let diff_istr = diff Gwdb.compare_istr
let diff_int = diff Int.compare
let diff_istr_list = diff_list Gwdb.compare_istr
let diff_cdate =
  let cmp_cdate (d1 : Def.cdate) (d2 : Def.cdate) = if d1 = d2 then 0 else 1 in
  diff cmp_cdate


let diff_first_name p n =
  diff_istr p.Def.first_name n.Def.first_name
let diff_surname p n =
  diff_istr p.Def.surname n.Def.surname
let diff_occ p n =
  diff_int p.Def.occ n.Def.occ
let diff_public_name p n =
  diff_istr p.Def.public_name n.Def.public_name
let diff_qualifiers p n =
  diff_istr_list p.Def.qualifiers n.Def.qualifiers
let diff_aliases p n =
  diff_istr_list p.Def.aliases n.Def.aliases
let diff_fn_aliases p n =
  diff_istr_list p.Def.first_names_aliases n.Def.first_names_aliases
let diff_sn_aliases p n =
  diff_istr_list p.Def.surnames_aliases n.Def.surnames_aliases
let diff_occupation p n =
  diff_istr p.Def.occupation n.Def.occupation
let diff_sex p n =
  let int_of_sex = function
    | Def.Male -> 0
    | Female -> 1
    | Neuter -> 3
  in
  let cmp_sex s1 s2 = Int.compare (int_of_sex s1) (int_of_sex s2) in
  diff cmp_sex p.Def.sex n.Def.sex
let diff_birth p n =
  diff_cdate p.Def.birth n.Def.birth
let diff_baptism p n =
  diff_cdate p.Def.baptism n.Def.baptism
let diff_death p n =
  let cmp_death d1 d2 = if d1 = d2 then 0 else 1 in
  diff cmp_death p.Def.death n.Def.death
let diff_burial p n =
  let cmp_burial b1 b2 = if b1 = b2 then 0 else 1 in
  diff cmp_burial p.Def.burial n.Def.burial
let diff_birth_place p n =
  diff_istr p.Def.birth_place n.Def.birth_place
let diff_burial_place p n =
  diff_istr p.Def.burial_place n.Def.burial_place
let diff_baptism_place p n =
  diff_istr p.Def.baptism_place n.Def.baptism_place
let diff_death_place p n =
  diff_istr p.Def.death_place n.Def.death_place


let diff_person ~base ~previously ~now =
  let p = previously in
  let n = now in
  let _ = base in
  (*print_endline @@ "FN: " ^ (Gwdb.string_of_istr p.Def.first_name) ^ " "
                  ^ (Gwdb.string_of_istr n.Def.first_name);
  let diff_fn = diff_istr p.Def.first_name n.Def.first_name in
  let s = diff_to_string (fun istr -> Gwdb.sou base istr) diff_fn in
  print_endline s;
  let diff_sn = diff_istr p.Def.surname n.Def.surname in
  let s = diff_to_string (fun istr -> Gwdb.sou base istr) diff_sn in
  print_endline s;
  *)
  let first_name = diff_first_name p n in
  let surname = diff_surname p n in
  let occ = diff_occ p n in
  let public_name = diff_public_name p n in
  let qualifiers = diff_qualifiers p n in
  let aliases = diff_aliases p n in
  let first_names_aliases = diff_fn_aliases p n in
  let surnames_aliases = diff_sn_aliases p n in
  let occupation = diff_occupation p n in
  let sex = diff_sex p n in
  let birth = diff_birth p n in
  let death = diff_death p n in
  let baptism = diff_baptism p n in
  let burial = diff_burial p n in
  let birth_place = diff_birth_place p n in
  let death_place = diff_death_place p n in
  let baptism_place = diff_baptism_place p n in
  let burial_place = diff_burial_place p n in
  let _diffp = no_diff_person in
  let titles = None in
  let rparents = None in
  {
    first_name;
    surname;
    occ;
    public_name;
    qualifiers;
    aliases;
    first_names_aliases;
    surnames_aliases;
    occupation;
    sex;
    birth;
    baptism;
    death;
    burial;
    birth_place;
    baptism_place;
    death_place;
    burial_place;
    titles;
    rparents;
  }
