
type person_reference = {
  first_name : string;
  surname: string;
  occ: int
}

type event_data = {
  date: Def.cdate;
  place: string;
}

type death_data = {
  death: Def.death;
  death_place : string;
}

type burial_data = {
  burial : Def.burial;
  burial_place : string;
}

type union_data = (person_reference * Def.cdate) list

type ascends_data = {
  father : person_reference option;
  mother : person_reference option;
}
type children_data = person_reference list

type person_data = {
  reference : person_reference;
  public_name : string;
  qualifiers : string list;
  aliases : string list;
  first_names_aliases : string list;
  surnames_aliases : string list;
  father : person_reference option;
  mother : person_reference option;
  occupation : string;
  sex : Def.sex;
  birth : event_data;
  baptism : event_data;
  death : death_data;
  burial : burial_data;
  unions : union_data;
  children : children_data;
  access : Def.access
}

type t =
    Add of person_data
  | Update of person_reference * person_data
  | Delete of person_reference


let was_empty_str istr_diff_o = match istr_diff_o with
    None -> false
  | Some istr_diff ->
    match istr_diff.Diff_types.previously with
    | Some istr -> Gwdb.is_empty_string istr
    | None -> true

let is_now_quest_str istr_diff_o = match istr_diff_o with
    None -> false
  | Some istr_diff ->
    match istr_diff.Diff_types.now with
    | Some istr -> Gwdb.is_quest_string istr
    | None -> true

let is_new_person (person_diff : Diff_types.Person_diff.t) =
  was_empty_str person_diff.first_name
  && was_empty_str person_diff.surname
    
let is_deleted (person_diff : Diff_types.Person_diff.t) =
  is_now_quest_str person_diff.first_name
  && is_now_quest_str person_diff.surname
  

let previous_istr_to_string base default (diff : Gwdb.istr Diff_types.diff option)= match diff with
  | Some diff -> begin match diff.previously with
      | Some istr -> Gwdb.sou base istr
      | None -> default ()
    end
  | None -> default ()

let previous_occ default = function
  | Some Diff_types.{previously = Some p; now = _now} -> p
  | _ -> default ()

let get_current_fn base diff =
  Gwdb.poi base diff.Diff_types.Person_diff.iper |> Gwdb.get_first_name |> Gwdb.sou base
let get_current_sn base diff =
  Gwdb.poi base diff.Diff_types.Person_diff.iper |> Gwdb.get_surname |> Gwdb.sou base
let get_current_occ base diff =
  Gwdb.poi base diff.Diff_types.Person_diff.iper |> Gwdb.get_occ

let person_ref_opt base iper =
  if Gwdb.eq_iper Gwdb.dummy_iper iper then None
  else
    let p = Gwdb.poi base iper in
    Some {
      first_name = Gwdb.get_first_name p |> Gwdb.sou base;
      surname = Gwdb.get_surname p |> Gwdb.sou base;
      occ = Gwdb.get_occ p
    }

let previous_person_reference base (diff : Diff_types.Person_diff.t) =
  let first_name = previous_istr_to_string base (fun _ -> get_current_fn base diff) diff.first_name in
  let surname = previous_istr_to_string base (fun _ -> get_current_sn base diff) diff.surname in
  let occ = previous_occ (fun _ -> get_current_occ base diff) diff.occ in
  {first_name; surname; occ}

let _current_person_reference base diff =
  let first_name = get_current_fn base diff in
  let surname = get_current_sn base diff in
  let occ = get_current_occ base diff in
  {first_name; surname; occ}

let person_data base diff =
  let iper = diff.Diff_types.Person_diff.iper in
  let p = Gwdb.poi base iper in
  let first_name = Gwdb.sou base @@ Gwdb.get_first_name p in
  let surname = Gwdb.sou base @@ Gwdb.get_surname p in
  let occ = Gwdb.get_occ p in
  let reference = {first_name; surname; occ} in
  let ascends = Gwdb.gen_ascend_of_person p in
  let ifather, imother = match ascends with
    | { parents = Some parents; _} ->
      let fam = Gwdb.foi base parents in
      Gwdb.get_father fam, Gwdb.get_mother fam
    | _ -> Gwdb.dummy_iper, Gwdb.dummy_iper
  in
  let father_ref, mother_ref = person_ref_opt base ifather, person_ref_opt base imother in
  let occupation = Gwdb.get_occupation p |> Gwdb.sou base in
  let sex = Gwdb.get_sex p in
  let birth = {
    place = Gwdb.get_birth_place p |> Gwdb.sou base;
    date = Gwdb.get_birth p;
  } in
  let baptism = {
    place = Gwdb.get_baptism_place p |> Gwdb.sou base;
    date = Gwdb.get_baptism p;
  }
  in
  let burial = {
    burial_place = Gwdb.get_burial_place p |> Gwdb.sou base;
    burial = Gwdb.get_burial p;
  }
  in
  let death = {
    death_place = Gwdb.get_death_place p |> Gwdb.sou base;
    death = Gwdb.get_death p
  }
  in
  let unions =
    Gwdb.gen_union_of_person p |> fun unions ->
    Array.fold_left (fun acc ifam ->
        let fam = Gwdb.foi base ifam in
        let fath = Gwdb.get_father fam in
        let moth = Gwdb.get_mother fam in
        let sp = if Gwdb.eq_iper iper fath then moth else fath in
        let sp_ref_opt = person_ref_opt base sp in
        let d = Gwdb.get_marriage fam in
        match sp_ref_opt with
        | Some sp_ref -> (sp_ref, d) :: acc
        | None -> acc
      ) [] unions.Def.family
  in
  let children =
    Gwdb.gen_union_of_person p |> fun unions ->
    Array.fold_left (fun acc ifam ->
        let fam = Gwdb.foi base ifam in
        let children = Gwdb.get_children fam in
        Array.fold_left (fun acc ipc ->
            match person_ref_opt base ipc with
            | Some childref -> childref :: acc
            | None -> acc
          ) acc children
      ) [] unions.Def.family
  in
  {
    reference;
    public_name = "";
    qualifiers = [];
    aliases = [];
    first_names_aliases = [];
    surnames_aliases = [];
    father = father_ref;
    mother = mother_ref;
    occupation;
    sex;
    birth;
    baptism;
    burial;
    unions;
    children;
    death;
    access = Gwdb.get_access p
  }

let string_of_death_reason = function
  | Def.Killed -> "Killed"
  | Murdered -> "Murdered"
  | Executed -> "Executed"
  | Disappeared -> "Disappeared"
  | Unspecified -> "Unspecified"

let string_of_death = function
  | Def.NotDead -> "not dead"
  | Death (dr, d) -> string_of_death_reason dr ^ " " ^ Diff_utils.date_to_string d
  | DeadYoung -> "dead young"
  | DeadDontKnowWhen -> "DeadDontKnowWhen"
  | DontKnowIfDead -> "DontKnowIfDead"
  | OfCourseDead -> "OfCourseDead"

let string_of_sex = function
  | Def.Male -> "M"
  | Female -> "F"
  | Neuter -> "N"

let string_of_access = function
  | Def.IfTitles -> "IfTitles"
  | Public -> "Public"
  | Private -> "Private"

let string_of_burial = function
  | Def.UnknownBurial -> "UnknownBurial"
  | Buried cdate -> "Buried " ^ Diff_utils.date_to_string cdate
  | Cremated cdate -> "Cremated " ^ Diff_utils.date_to_string cdate

let string_of_reference = function
  | {first_name = fn; surname = sn; occ = oc} ->
    String.concat "_" [fn; sn; string_of_int oc]

let string_of_person_data data =
  ["REFERENCE", string_of_reference data.reference;
   "OCCUPATION", data.occupation;
   "SEX", string_of_sex data.sex;
   "FATHER", (match data.father with Some r -> string_of_reference r | None -> "");
   "MOTHER", (match data.mother with Some r -> string_of_reference r | None -> "");
   "UNIONS", List.map (fun (u_ref, d) -> string_of_reference u_ref ^  " " ^ Diff_utils.date_to_string d) data.unions |> String.concat "; ";
   "CHILDREN", List.map (fun (cref) -> string_of_reference cref) data.children |> String.concat "; ";
   "BIRTH", data.birth.place ^ " " ^ Diff_utils.date_to_string data.birth.date;
   "BAPTISM", data.baptism.place ^ " " ^ Diff_utils.date_to_string data.baptism.date;
   "DEATH", string_of_death data.death.death ^ " " ^ data.death.death_place;
   "BURIAL", string_of_burial data.burial.burial ^ " " ^ data.burial.burial_place;
   "ACCESS", string_of_access data.access
  ] |>
  List.map (fun (tag, value) -> if value = "" then tag ^ ": EMPTY" else tag ^ ": " ^ value)
  |> String.concat "\n"

let string_of_command = function
  | Add data -> "ADD\n" ^ string_of_person_data data ^ "\nEND\n"
  | Update (person_reference, data) -> "UPDATE\n" ^ string_of_reference person_reference ^ "\n" ^ string_of_person_data data ^ "\nEND\n"
  | Delete reference -> "DELETE " ^ string_of_reference reference ^ "\n"

let command_of_person_diff base person_diff =
  let c = if is_deleted person_diff then begin
      (*    print_endline "prod del";*)
    Delete (previous_person_reference base person_diff)
  end
  else if is_new_person person_diff then begin
    (*print_endline "prod add";*)
    Add (person_data base person_diff)
  end
  else begin
    (*print_endline "prod update";*)
    Update (previous_person_reference base person_diff, person_data base person_diff)
  end
  in
  c

let int_of_cmd = function
  | Delete _ -> 0
  | Update _ -> 1
  | Add _ ->  2


let cmp_command c1 c2 =
  int_of_cmd c1 - int_of_cmd c2
