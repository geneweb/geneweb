type person_desc = {
  first_name : string;
  surname : string;
  occ : int;
  sex : Def.sex;
  mutable parents : int option;
  mutable unions : int list;
}

type family_desc = {
  father : int;
  mother : int;
  mutable children : int list;
  fsources : string;
}

type t = {
  mutable strings : string list;
  mutable persons : person_desc list;
  mutable families : family_desc list;
}

let create () = { strings = [ ""; "?" ]; persons = []; families = [] }

let add_string db s =
  let rec find idx = function
    | [] -> None
    | x :: _ when String.equal x s -> Some idx
    | _ :: tl -> find (idx + 1) tl
  in
  match find 0 db.strings with
  | Some idx -> idx
  | None ->
      let idx = List.length db.strings in
      db.strings <- db.strings @ [ s ];
      idx

(* --- Internal Helpers --- *)

let get_person_opt db id =
  if id >= 0 && id < List.length db.persons then Some (List.nth db.persons id)
  else None

let get_family_opt db id =
  if id >= 0 && id < List.length db.families then Some (List.nth db.families id)
  else None

let add_union db p_id fam_id =
  Option.iter
    (fun p ->
      if not (List.mem fam_id p.unions) then p.unions <- p.unions @ [ fam_id ])
    (get_person_opt db p_id)

let add_child db fam_id ch_id =
  Option.iter
    (fun f ->
      if not (List.mem ch_id f.children) then
        f.children <- f.children @ [ ch_id ])
    (get_family_opt db fam_id)

let set_parent_link db p_id fam_id =
  Option.iter (fun p -> p.parents <- Some fam_id) (get_person_opt db p_id)

(* --- Public API --- *)

let add_person db ?(first_name = "") ?(surname = "") ?(occ = 0)
    ?(sex = Def.Neuter) ?parents () =
  let p_id = List.length db.persons in
  let p = { first_name; surname; occ; sex; parents; unions = [] } in
  db.persons <- db.persons @ [ p ];
  Option.iter (fun fam_id -> add_child db fam_id p_id) parents;
  p_id

let add_family db ?(father = -1) ?(mother = -1) ?(children = [])
    ?(fsources = "") () =
  let fam_id = List.length db.families in
  let fam = { father; mother; children; fsources } in
  db.families <- db.families @ [ fam ];
  add_union db father fam_id;
  add_union db mother fam_id;
  List.iter (fun ch_id -> set_parent_link db ch_id fam_id) children;
  fam_id

let set_unions db ~person unions =
  Option.iter (fun p -> p.unions <- unions) (get_person_opt db person)

let set_parents db ~person parents =
  Option.iter (fun p -> p.parents <- parents) (get_person_opt db person)

let set_children db ~family children =
  Option.iter (fun f -> f.children <- children) (get_family_opt db family)

(* --- Build Helpers --- *)

let build_person_arrays db get_str_idx =
  let persons_len = List.length db.persons in
  let persons_arr =
    Array.init persons_len (fun i ->
        let p = List.nth db.persons i in
        {
          (Mutil.empty_person 0 1) with
          Def.first_name = get_str_idx p.first_name;
          surname = get_str_idx p.surname;
          occ = p.occ;
          sex = p.sex;
          key_index = i;
        })
  in
  let ascends_arr =
    Array.init persons_len (fun i ->
        let p = List.nth db.persons i in
        { Gwdb.no_ascend with Def.parents = p.parents })
  in
  let unions_arr =
    Array.init persons_len (fun i ->
        let p = List.nth db.persons i in
        { Def.family = Array.of_list p.unions })
  in
  (persons_arr, ascends_arr, unions_arr)

let build_family_arrays db get_str_idx =
  let families_len = List.length db.families in
  let families_arr =
    Array.init families_len (fun i ->
        let f = List.nth db.families i in
        let fsources = if f.fsources = "" then 0 else get_str_idx f.fsources in
        { (Mutil.empty_family 0) with Def.fam_index = i; fsources })
  in
  let couples_arr =
    Array.init families_len (fun i ->
        let f = List.nth db.families i in
        Adef.couple f.father f.mother)
  in
  let descends_arr =
    Array.init families_len (fun i ->
        let f = List.nth db.families i in
        { Def.children = Array.of_list f.children })
  in
  (families_arr, couples_arr, descends_arr)

let build ?(bname = "") db =
  let get_str_idx = function "" -> 0 | "?" -> 1 | s -> add_string db s in
  let p_data = build_person_arrays db get_str_idx in
  let f_data = build_family_arrays db get_str_idx in
  let strings_arr = Array.of_list db.strings in
  let base_notes =
    { Def.nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }
  in
  let base = Gwdb.make bname [] (p_data, f_data, strings_arr, base_notes) in
  Gwdb.load_ascends_array base;
  Gwdb.load_unions_array base;
  Gwdb.load_couples_array base;
  Gwdb.load_descends_array base;
  Gwdb.load_strings_array base;
  Gwdb.load_persons_array base;
  Gwdb.load_families_array base;
  base

let iper (i : int) : Gwdb.iper = i |> Int.to_string |> Gwdb.iper_of_string
let ifam (i : int) : Gwdb.ifam = i |> Int.to_string |> Gwdb.ifam_of_string
let istr (i : int) : Gwdb.istr = i |> Int.to_string |> Gwdb.istr_of_string
let poi base i = Gwdb.poi base (iper i)
let foi base i = Gwdb.foi base (ifam i)
let sou base i = Gwdb.sou base (istr i)
