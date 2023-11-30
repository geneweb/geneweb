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


let _diff_o cmp o o' = match o, o' with
  | Some v, Some v' -> diff cmp v v'
  | Some _ , None -> Some (make_diff o None)
  | None, Some _ -> Some (make_diff None o')
  | None, None -> None

let diff_npoc p n =
  let fn_diff = diff_first_name p n in
  let sn_diff = diff_surname p n in
  let occ_diff = diff_occ p n in
  match fn_diff, sn_diff, occ_diff with
  | None, None, None -> None
  | _ -> Some Npoc_diff.{first_name = fn_diff; surname = sn_diff; occ = occ_diff}


let diff_father base fam_p fam_n =
  let fath_p =
    Gwdb.poi base (Gwdb.get_father_baseonly fam_p)
    |> Gwdb.gen_person_of_person_baseonly
  in
  let fath_n =
    Gwdb.poi base (Gwdb.get_father fam_n)
    |> Gwdb.gen_person_of_person
  in
  diff_npoc fath_p fath_n

let diff_mother base fam_p fam_n =
  let moth_p =
    Gwdb.poi base (Gwdb.get_mother_baseonly fam_p)
    |> Gwdb.gen_person_of_person_baseonly
  in
  let moth_n =
    Gwdb.poi base (Gwdb.get_mother fam_n)
    |> Gwdb.gen_person_of_person
  in
  diff_npoc moth_p moth_n

let diff_ascends base ifam_p ifam_n =
  let fam_p = Gwdb.foi base ifam_p in
  let fam_n = Gwdb.foi base ifam_n in
  let father = diff_father base fam_p fam_n in
  let mother = diff_mother base fam_p fam_n in
  if Option.is_some father || Option.is_some mother then
    Some Diff_types.Ascend_diff.{father; mother;}
  else None
  
let new_ascends base ifam =
  let fam = Gwdb.foi base ifam in
  let fath = Gwdb.poi base (Gwdb.get_father fam) in
  let moth = Gwdb.poi base (Gwdb.get_mother fam) in
  let father = Some Diff_types.Npoc_diff.{
      first_name = Some (make_diff None (Some (Gwdb.get_first_name fath)));
      surname = Some (make_diff None (Some (Gwdb.get_surname fath)));
      occ = Some (make_diff None (Some (Gwdb.get_occ fath)));
    }
  in
  let mother = Some Diff_types.Npoc_diff.{
      first_name = Some (make_diff None (Some (Gwdb.get_first_name moth)));
      surname = Some (make_diff None (Some (Gwdb.get_surname moth)));
      occ = Some (make_diff None (Some (Gwdb.get_occ moth)));
    }
  in
  Some Diff_types.Ascend_diff.{father; mother;}
  
let ascends_removed base ifam =
  let fam = Gwdb.foi base ifam in
  let fath =
    Gwdb.poi base (Gwdb.get_father_baseonly fam)
  |> Gwdb.gen_person_of_person_baseonly
  in
  let moth =
    Gwdb.poi base (Gwdb.get_mother_baseonly fam)
    |> Gwdb.gen_person_of_person_baseonly
  in
  let father = Some Diff_types.Npoc_diff.{
      first_name = Some (make_diff (Some fath.first_name) None);
      surname = Some (make_diff (Some fath.surname) None);
      occ = Some (make_diff (Some fath.occ) None);
    }
  in
  let mother = Some Diff_types.Npoc_diff.{
      first_name = Some (make_diff (Some moth.first_name) None);
      surname = Some (make_diff (Some moth.surname) None);
      occ = Some (make_diff (Some moth.occ) None);
    }
  in
  Some Diff_types.Ascend_diff.{father; mother;}
  
let diff_ascends base iper =
  let p = Gwdb.poi base iper in
  let n = Gwdb.poi base iper in
  let asc_p = Gwdb.gen_ascend_of_person_baseonly p in
  let asc_n = Gwdb.gen_ascend_of_person n in
  match asc_p.parents, asc_n.parents with
  | Some ifam_p, Some ifam_n -> diff_ascends base ifam_p ifam_n
  | None, Some ifam -> new_ascends base ifam
  | Some ifam, None -> ascends_removed base ifam
  | None, None -> None

let diff_unions _p _n = None

module IperSet =
  Set.Make (struct
      type t = Gwdb.iper
      let compare = Gwdb.compare_iper
    end)


let diff_children base iper =
  let p = Gwdb.poi base iper in
  let p' = Gwdb.poi base iper in
  let families_p = Array.map (Gwdb.foi base) (Gwdb.get_family_baseonly p) in
  let families_n = Array.map (Gwdb.foi base) (Gwdb.get_family p') in
  let children_p = Array.map Gwdb.get_children_baseonly families_p in
  let children_n = Array.map Gwdb.get_children families_n in
  let children_p_set = Array.fold_left (fun set arr ->
      Array.fold_left (fun set iper -> IperSet.add iper set) set arr
    ) IperSet.empty children_p
  in
  let children_n_set =
    Array.fold_left (fun set arr ->
        Array.fold_left (fun set iper -> IperSet.add iper set) set arr
      ) IperSet.empty children_n
  in
  if IperSet.compare children_p_set children_n_set <> 0 then
    Some (make_diff (list_o (IperSet.elements children_p_set)) (list_o (IperSet.elements children_n_set)))
  else None
  
  

let diff_person ~base ~iper ~previously ~now =
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
  let unions = diff_unions p n in
  let children = diff_children base iper in
  let ascends = diff_ascends base iper in
  Diff_types.Person_diff.{
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
    unions;
    ascends;
    children;
  }
