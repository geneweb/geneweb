open Diff_types

module IperSet =
  Set.Make (struct
      type t = Gwdb.iper
      let compare = Gwdb.compare_iper
    end)

module IfamSet =
  Set.Make (struct
      type t = Gwdb.ifam
      let compare = Gwdb.compare_ifam
    end)

let log = print_endline
(*let log _ = ()*)
let try_with_failure f default x = try f x with Failure _ -> default

let get_family_baseonly =
  try_with_failure Gwdb.get_family_baseonly [||]

let gen_person_of_person_baseonly base iper =
  try_with_failure Gwdb.gen_person_of_person_baseonly (Gwdb.empty_person base iper |> Gwdb.gen_person_of_person)

let gen_union_of_person_baseonly =
  try_with_failure Gwdb.gen_union_of_person_baseonly Def.{family = [||]}

let gen_ascend_of_person_baseonly =
  try_with_failure Gwdb.gen_ascend_of_person_baseonly Def.{parents = None; consang = Adef.no_consang}  

let gen_family_of_family_baseonly base ifam =
  try_with_failure Gwdb.gen_family_of_family_baseonly (Gwdb.empty_family base ifam |> Gwdb.gen_family_of_family)

let get_father_baseonly =
  try_with_failure Gwdb.get_father_baseonly Gwdb.dummy_iper
let get_mother_baseonly =
  try_with_failure Gwdb.get_mother_baseonly Gwdb.dummy_iper

let get_children_baseonly =
  try_with_failure Gwdb.get_children_baseonly [||]  

module Utils = struct
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
end

let diff_first_name p n =
  Utils.diff_istr p.Def.first_name n.Def.first_name
let diff_surname p n =
  Utils.diff_istr p.Def.surname n.Def.surname
let diff_occ p n =
  Utils.diff_int p.Def.occ n.Def.occ
let diff_public_name p n =
  Utils.diff_istr p.Def.public_name n.Def.public_name
let diff_qualifiers p n =
  Utils.diff_istr_list p.Def.qualifiers n.Def.qualifiers
let diff_aliases p n =
  Utils.diff_istr_list p.Def.aliases n.Def.aliases
let diff_fn_aliases p n =
  Utils.diff_istr_list p.Def.first_names_aliases n.Def.first_names_aliases
let diff_sn_aliases p n =
  Utils.diff_istr_list p.Def.surnames_aliases n.Def.surnames_aliases
let diff_occupation p n =
  Utils.diff_istr p.Def.occupation n.Def.occupation
let diff_sex p n =
  let int_of_sex = function
    | Def.Male -> 0
    | Female -> 1
    | Neuter -> 3
  in
  let cmp_sex s1 s2 = Int.compare (int_of_sex s1) (int_of_sex s2) in
  Utils.diff cmp_sex p.Def.sex n.Def.sex
let diff_birth p n =
  Utils.diff_cdate p.Def.birth n.Def.birth
let diff_baptism p n =
  Utils.diff_cdate p.Def.baptism n.Def.baptism
let diff_death p n =
  let cmp_death d1 d2 = if d1 = d2 then 0 else 1 in
  Utils.diff cmp_death p.Def.death n.Def.death
let diff_burial p n =
  let cmp_burial b1 b2 = if b1 = b2 then 0 else 1 in
  Utils.diff cmp_burial p.Def.burial n.Def.burial
let diff_birth_place p n =
  Utils.diff_istr p.Def.birth_place n.Def.birth_place
let diff_burial_place p n =
  Utils.diff_istr p.Def.burial_place n.Def.burial_place
let diff_baptism_place p n =
  Utils.diff_istr p.Def.baptism_place n.Def.baptism_place
let diff_death_place p n =
  Utils.diff_istr p.Def.death_place n.Def.death_place

(*
let _diff_o cmp o o' = match o, o' with
  | Some v, Some v' -> diff cmp v v'
  | Some _ , None -> Some (make_diff o None)
  | None, Some _ -> Some (make_diff None o')
  | None, None -> None
*)
let diff_npoc p n =
  let fn_diff = diff_first_name p n in
  let sn_diff = diff_surname p n in
  let occ_diff = diff_occ p n in
  match fn_diff, sn_diff, occ_diff with
  | None, None, None -> None
  | _ -> Some Npoc_diff.{first_name = fn_diff; surname = sn_diff; occ = occ_diff}


let diff_father base fam_p fam_n =
  log "diff_fath";  
  let fath_p =
    Gwdb.poi base (get_father_baseonly fam_p)
    |> Gwdb.gen_person_of_person_baseonly
  in
  let fath_n =
    Gwdb.poi base (Gwdb.get_father fam_n)
    |> Gwdb.gen_person_of_person
  in
  diff_npoc fath_p fath_n

let diff_mother base fam_p fam_n =
  log "diff_moth";
  let moth_p =
    Gwdb.poi base (get_mother_baseonly fam_p)
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
      first_name = Some (Utils.make_diff None (Some (Gwdb.get_first_name fath)));
      surname = Some (Utils.make_diff None (Some (Gwdb.get_surname fath)));
      occ = Some (Utils.make_diff None (Some (Gwdb.get_occ fath)));
    }
  in
  let mother = Some Diff_types.Npoc_diff.{
      first_name = Some (Utils.make_diff None (Some (Gwdb.get_first_name moth)));
      surname = Some (Utils.make_diff None (Some (Gwdb.get_surname moth)));
      occ = Some (Utils.make_diff None (Some (Gwdb.get_occ moth)));
    }
  in
  Some Diff_types.Ascend_diff.{father; mother;}
  
let ascends_removed base ifam =
  let fam = Gwdb.foi base ifam in
  let fath =
    Gwdb.poi base (get_father_baseonly fam)
  |> Gwdb.gen_person_of_person_baseonly
  in
  let moth =
    Gwdb.poi base (get_mother_baseonly fam)
    |> Gwdb.gen_person_of_person_baseonly
  in
  let father = Some Diff_types.Npoc_diff.{
      first_name = Some (Utils.make_diff (Some fath.first_name) None);
      surname = Some (Utils.make_diff (Some fath.surname) None);
      occ = Some (Utils.make_diff (Some fath.occ) None);
    }
  in
  let mother = Some Diff_types.Npoc_diff.{
      first_name = Some (Utils.make_diff (Some moth.first_name) None);
      surname = Some (Utils.make_diff (Some moth.surname) None);
      occ = Some (Utils.make_diff (Some moth.occ) None);
    }
  in
  Some Diff_types.Ascend_diff.{father; mother;}
  
let diff_ascends base iper =
  log "diff_ascends";
  let p = Gwdb.poi base iper in
  let n = Gwdb.poi base iper in
  let asc_p = gen_ascend_of_person_baseonly p in
  let asc_n = Gwdb.gen_ascend_of_person n in
  match asc_p.parents, asc_n.parents with
  | Some ifam_p, Some ifam_n -> diff_ascends base ifam_p ifam_n
  | None, Some ifam -> new_ascends base ifam
  | Some ifam, None -> ascends_removed base ifam
  | None, None -> None

module UnionSet = Set.Make(struct
    type t = Gwdb.iper * Def.cdate option
    let compare (ip, d) (ip',d') =
      let c = Gwdb.compare_iper ip ip' in
      if c = 0 then
        if d = d' then 0 else 1
      else c
  end)

let diff_unions base iper =
  log "diff_unions";
  let other father mother ifam =
    let fam = Gwdb.foi base ifam in
    let fath = father fam in
    let moth = mother fam in
    if Gwdb.eq_iper fath iper then moth else fath
  in
  let pdate_of ifam fam = (gen_family_of_family_baseonly base ifam fam).Def.marriage in
  let ndate_of fam = (Gwdb.gen_family_of_family fam).marriage in
  let other_p = other get_father_baseonly get_mother_baseonly in
  let other_n = other Gwdb.get_father Gwdb.get_mother in
  let p = Gwdb.poi base iper in
  let p' = Gwdb.poi base iper in
  let unions_p = gen_union_of_person_baseonly p in
  let unions_n = Gwdb.gen_union_of_person p' in
  let iperset_p = Array.fold_left (fun set ifam ->
      log @@ "UNION_P" ^ Gwdb.string_of_iper (other_p ifam);
      let fam = Gwdb.foi base ifam in
      let d = pdate_of ifam fam in
      UnionSet.add (other_p ifam, Some d) set)
      UnionSet.empty unions_p.family
  in
  let iperset_n = Array.fold_left (fun set ifam ->
      log @@ "UNION_N" ^ Gwdb.string_of_iper (other_p ifam);
      let fam = Gwdb.foi base ifam in
      let d = ndate_of fam in
      UnionSet.add (other_n ifam, Some d) set)
      UnionSet.empty unions_n.family
  in
  if UnionSet.compare iperset_p iperset_n <> 0 then
    Some (Utils.make_diff
            (Utils.list_o (UnionSet.elements iperset_p))
            (Utils.list_o (UnionSet.elements iperset_n))
         )
  else None

let diff_children base iper =
  log "diff_children";
  let p = Gwdb.poi base iper in
  let p' = Gwdb.poi base iper in
  let families_p = Array.map (Gwdb.foi base) (get_family_baseonly p) in
  let families_n = Array.map (Gwdb.foi base) (Gwdb.get_family p') in
  let children_p = Array.map get_children_baseonly families_p in
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
    Some (Utils.make_diff
            (Utils.list_o (IperSet.elements children_p_set))
            (Utils.list_o (IperSet.elements children_n_set)))
  else None
  
  

let diff_person ~base ~iper ~previously ~now =
  let p = previously in
  let n = now in
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
  let titles = None in
  let rparents = None in
  let unions = diff_unions base iper in
  let children = diff_children base iper in
  let ascends = diff_ascends base iper in
  Diff_types.Person_diff.{
    iper;
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


let diff_to_string to_string diff =
  match diff with
  | Some diff ->
    let p = Option.map to_string diff.Diff_types.previously in
    let n = Option.map to_string diff.now in
    begin match p, n with
        Some p, Some n -> "P:" ^ p ^ " N:" ^ n
      | Some p, _ -> "P:" ^ p
      | _, Some n -> "N:" ^ n
      | None, None -> "No diff"
    end
  | None -> "No diff"


let diff_npoc_to_string diff_npoc = match diff_npoc with
  | Some diff_npoc ->
    let fn_s = diff_to_string Gwdb.string_of_istr diff_npoc.Diff_types.Npoc_diff.first_name in
    let sn_s = diff_to_string Gwdb.string_of_istr diff_npoc.surname in
    let occ_s = diff_to_string string_of_int diff_npoc.occ in
    "{\nfirst_name: " ^ fn_s ^ "\n" ^
    "surname: " ^ sn_s ^ "\n" ^
    "occ: " ^ occ_s ^ "\n}"
  | None -> "nodiff"

let diff_ascends_to_string diff_ascend = match diff_ascend with
  | Some diff_ascend ->
    let fath = diff_npoc_to_string diff_ascend.Diff_types.Ascend_diff.father in
    let moth = diff_npoc_to_string diff_ascend.mother in
    "{\nfather:\n" ^ fath ^ "\n{\nmother:\n" ^ moth
  | None -> "nodiff"

let diff_descend_to_string diff_descend =
  diff_to_string (fun l -> String.concat "," (List.map Gwdb.string_of_iper l)) diff_descend

let diff_unions_to_string diff_unions =
  diff_to_string (fun l -> String.concat "," (
      List.map (fun (ip, d) ->
          Gwdb.string_of_iper ip ^
          match d with Some d -> " " ^ Diff_utils.date_to_string d | None -> ""
        ) l
    )) diff_unions 

let diff_person_to_string diff_person =
  let open Diff_types.Person_diff in
  let fn_s = diff_to_string Gwdb.string_of_istr diff_person.first_name in
  let sn_s = diff_to_string Gwdb.string_of_istr diff_person.surname in
  let occ_s = diff_to_string string_of_int diff_person.occ in
  let public_s = diff_to_string Gwdb.string_of_istr diff_person.public_name in
  let qualifiers_s = diff_to_string (fun l -> List.map Gwdb.string_of_istr l |> String.concat ";") diff_person.qualifiers in
  let birth_s = diff_to_string Diff_utils.date_to_string diff_person.birth in
  let ascends_s = diff_ascends_to_string diff_person.ascends in
  let descend_s = diff_descend_to_string diff_person.children in
  let unions_s = diff_unions_to_string diff_person.unions in
  "{\nfirst_name: " ^ fn_s ^ "\n" ^
  "surname: " ^ sn_s ^ "\n" ^
  "occ: " ^ occ_s ^ "\n" ^
  "public_name:" ^ public_s ^ "\n" ^
  "qualifiers:" ^ qualifiers_s ^ "\n" ^
  "birth:" ^ birth_s ^ "\n" ^
  "ascends:" ^ ascends_s ^ "\n" ^
  "descend:" ^ descend_s ^ "\n" ^
  "unions:" ^ unions_s ^ "\n" ^
  "}"

module Env : sig
  type t
  val empty : t
  val add_iper : Gwdb.base -> t -> Gwdb.iper -> t
  val add_ifam : Gwdb.base -> t -> Gwdb.ifam -> t
  val fold : ('a -> Gwdb.iper -> 'a) -> 'a -> t -> 'a
end = struct

  
  type t = {
    persons : IperSet.t;
    families : IfamSet.t;
  }

  let empty = {
    persons = IperSet.empty;
    families = IfamSet.empty;
  }

  let all_ipers_from_ifam base ifam =
    let fam = Gwdb.foi base ifam in
    let fam' = Gwdb.foi base ifam in
    let fath = get_father_baseonly fam in
    let fath' = Gwdb.get_father fam' in
    let moth = get_mother_baseonly fam in
    let moth' = Gwdb.get_mother fam' in
    let children = get_children_baseonly fam in
    let children' = Gwdb.get_children fam' in
(*    log "FATH";
    log @@ Gwdb.string_of_iper fath;
    log @@ Gwdb.string_of_iper fath';
    log "MOTH";
    log @@ Gwdb.string_of_iper moth;
    log @@ Gwdb.string_of_iper moth';
    log "CHLD";
    Array.iter (fun i -> log @@ Gwdb.string_of_iper i) children;
    log "CHLD'";
      Array.iter (fun i -> log @@ Gwdb.string_of_iper i) children';*)
    let iset = IperSet.empty in
    let iset = IperSet.add fath iset in
    let iset = IperSet.add moth iset in
    let iset = Array.fold_left (fun iset i -> IperSet.add i iset) iset children in
    let iset = IperSet.add fath' iset in
    let iset = IperSet.add moth' iset in
    let iset = Array.fold_left (fun iset i -> IperSet.add i iset) iset children' in
    iset
  
  let add_all_ipers_from_ifam base env ifam =
    let iperset = all_ipers_from_ifam base ifam in
    {env with persons = IperSet.union env.persons iperset}
  
  let add_all_related_ipers base env iper =
    let p = Gwdb.poi base iper in
    let p' = Gwdb.poi base iper in
    let fam_p = get_family_baseonly p in
    let fam_n = Gwdb.get_family p' in
    let ifamset = Array.fold_left (fun set ifam -> IfamSet.add ifam set) IfamSet.empty fam_p in
    let ifamset = Array.fold_left (fun set ifam -> IfamSet.add ifam set) ifamset fam_n in
    let env = IfamSet.fold (fun ifam env -> add_all_ipers_from_ifam base env ifam) ifamset env in
    env
  
  let add_iper base env iper =
    let env = add_all_related_ipers base env iper in
    {env with persons = IperSet.add iper env.persons}
  
  let add_ifam base env ifam =
    let env = add_all_ipers_from_ifam base env ifam in
    {env with families = IfamSet.add ifam env.families}

  let fold f acc env = IperSet.fold (fun iper acc -> f acc iper) env.persons acc
  
end

let compute_diff base iper =

  let p = Gwdb.poi base iper in
  let p' = Gwdb.poi base iper in

  let genp_base = gen_person_of_person_baseonly base iper p in
  
  let genp_patch = Gwdb.gen_person_of_person p' in

  log @@ "DBG" ^ (Gwdb.string_of_iper iper);
  let fn, sn = genp_base.first_name, genp_base.surname in
  let fn', sn' = genp_patch.first_name, genp_patch.surname in
  log (Gwdb.sou base fn ^ " " ^ Gwdb.sou base sn);
  log (Gwdb.sou base fn' ^ " " ^ Gwdb.sou base sn');
  let dp = diff_person ~base ~iper ~previously:genp_base ~now:genp_patch in
  let dp_s = diff_person_to_string dp in
  log dp_s;
  dp

let updates_from_patch base =
  let ipers = Gwdb.ipers_from_patch base in
  let ifams = Gwdb.ifams_from_patch  base in
  let env = Env.empty in
  log "=========================IFAMS==================================";
  let env = Gwdb.Collection.fold (Env.add_ifam base) env ifams in
  log "=========================IPERS==================================";
  let env = Gwdb.Collection.fold (Env.add_iper base) env ipers in
  Env.fold (fun l iper -> if not (Gwdb.eq_iper iper Gwdb.dummy_iper) then compute_diff base iper :: l else l) [] env

  
let _full_string_from_diff base (diff : Diff_types.Person_diff.t) =
  let _fn = Gwdb.sou base (Obj.magic diff.first_name) in
  assert false
