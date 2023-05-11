open Def
include Gwdb_driver

(** [insert_person base p a u]
    Add a new person with its union and ascendants in the [base].
    Allocate and returns the fresh new id for this person.
    [p] SHOULD be defined using [dummy_iper].
*)
let insert_person base p a u =
  let iper = Gwdb_driver.new_iper base in
  let p = { p with key_index = iper } in
  Gwdb_driver.insert_ascend base iper a;
  Gwdb_driver.insert_union base iper u;
  Gwdb_driver.insert_person base iper p;
  iper

(** [insert_family base f c d]
    Add a new family with its couple and descendants the in the [base].
    Allocate and returns the fresh new id for this family.
    [f] SHOULD be defined using [dummy_ifam].
*)
let insert_family base f c d =
  let ifam = Gwdb_driver.new_ifam base in
  Gwdb_driver.insert_family base ifam f;
  Gwdb_driver.insert_couple base ifam c;
  Gwdb_driver.insert_descend base ifam d;
  ifam

(** DELETE *)

let getp fn b i = fn @@ Gwdb_driver.poi b i
let get_gen_person = getp Gwdb_driver.gen_person_of_person
let get_gen_ascend = getp Gwdb_driver.gen_ascend_of_person
let get_gen_union = getp Gwdb_driver.gen_union_of_person
let getf fn b i = fn @@ Gwdb_driver.foi b i
let get_gen_family = getf Gwdb_driver.gen_family_of_family
let get_gen_couple = getf Gwdb_driver.gen_couple_of_family
let get_gen_descend = getf Gwdb_driver.gen_descend_of_family

let rec delete_person excl base ip =
  let iexcl, fexcl = excl in
  if ip = dummy_iper || List.mem ip iexcl then
    failwith
      ("gwdb.delete_person(" ^ string_of_iper ip ^ ",["
      ^ (List.map string_of_iper iexcl |> String.concat ",")
      ^ "])");
  let a = get_gen_ascend base ip in
  (* if person is the single child and their parents are empty persons
     then [ipers] contains father and mother and [ifams] contains family *)
  let ipers, ifams =
    match a.parents with
    | Some ifam ->
        (* delete ascendants *)
        Gwdb_driver.delete_ascend base ip;
        (* remove person id from family descendants *)
        let children =
          (get_gen_descend base ifam).children |> Mutil.array_except ip
        in
        Gwdb_driver.patch_descend base ifam { children };
        if children = [| ip |] then
          let c = get_gen_couple base ifam in
          let fath = Adef.father c in
          let moth = Adef.mother c in
          if is_empty_p base fath ~ifam && is_empty_p base moth ~ifam then
            ([ fath; moth ], [ ifam ])
          else ([], [])
        else ([], [])
    | None -> ([], [])
  in
  let del, ipers, ifams =
    let u = get_gen_union base ip in
    if u.family = [||] then (true, [], [])
    else
      Array.fold_left
        (fun (del, ipers, ifams) ifam ->
          let cpl = get_gen_couple base ifam in
          (* Test if ip is really in union in order to prevent "false positive" *)
          let fath = Adef.father cpl in
          let moth = Adef.mother cpl in
          if fath = ip || moth = ip then
            let d = get_gen_descend base ifam in
            if Array.length d.children > 1 then (false, ipers, ifams)
            else
              let sp = if ip = fath then moth else fath in
              if List.mem sp iexcl then (del, ipers, ifams)
              else if is_empty_p base sp ~ifam then
                (del, sp :: ipers, ifam :: ifams)
              else (false, ipers, ifams)
          else (
            (* Data are probably partially deleted.
               It is likely to happen when merging persons. *)
            rm_union base ifam ip;
            (del, ipers, ifams)))
        (true, ipers, ifams) u.family
  in
  if del then Gwdb_driver.delete_person base ip
  else
    Gwdb_driver.patch_person base ip
      { (no_person ip) with first_name = quest_string; surname = quest_string };
  let iexcl = if del then ip :: iexcl else iexcl in
  let excl = (iexcl, fexcl) in
  let excl =
    List.fold_left (fun excl ip -> delete_person excl base ip) excl ipers
  in
  List.fold_left (fun excl ifam -> delete_family excl base ifam) excl ifams

and is_empty_p ?ifam base sp =
  (get_gen_ascend base sp).parents = None
  && ((get_gen_union base sp).family
     = match ifam with Some i -> [| i |] | None -> [||])
  && get_gen_person base sp
     = { (no_person sp) with first_name = quest_string; surname = quest_string }

and delete_family excl base ifam =
  let iexcl, fexcl = excl in
  if ifam = dummy_ifam || List.mem ifam fexcl then
    failwith
      ("gwdb.delete_family(" ^ string_of_ifam ifam ^ ",["
      ^ (List.map string_of_ifam fexcl |> String.concat ",")
      ^ "])");
  let fam = foi base ifam in
  let fath = get_father fam in
  let moth = get_mother fam in
  let children = get_children fam in
  rm_union base ifam fath;
  rm_union base ifam moth;
  Array.iter (fun i -> patch_ascend base i no_ascend) children;
  Gwdb_driver.delete_family base ifam;
  Gwdb_driver.delete_couple base ifam;
  Gwdb_driver.delete_descend base ifam;
  let fexcl = ifam :: fexcl in
  let excl = (iexcl, fexcl) in
  let excl =
    if (not (List.mem fath iexcl)) && is_empty_p base fath then
      delete_person excl base fath
    else excl
  in
  let excl =
    if (not (List.mem moth iexcl)) && is_empty_p base moth then
      delete_person excl base moth
    else excl
  in
  Array.fold_left
    (fun excl i ->
      if (not (List.mem i iexcl)) && is_empty_p base i then
        delete_person excl base i
      else excl)
    excl children

and rm_union base ifam iper =
  { family = (get_gen_union base iper).family |> Mutil.array_except ifam }
  |> patch_union base iper

(** [delete_person base iper] and [delete_family base ifam]
    recursively delete data trying to do clever things:
    - if data to be deleted is linked and useful,
      it is replaced by empty data (and is actually deleted otherwise)
    - if empty data is linked to deleted data, the former is deleted as well
 *)
let delete_person base iper = ignore @@ delete_person ([], []) base iper

(** See {!val:delete_person}  *)
let delete_family base ifam = ignore @@ delete_family ([], []) base ifam

(**/**)

(** Misc *)

(** [nobtitles base allowed_titles denied_titles p] returns list of titles of a person [p]
    that apprears in [allowed_titles] and doesn't appears in [denied_titles]. If [allowed_titles]
    is empty the every title is allowed *)
let nobtitles base allowed_titles denied_titles p =
  let list = get_titles p in
  match Lazy.force allowed_titles with
  | [] -> list
  | allowed_titles -> (
      let list =
        List.fold_right
          (fun t l ->
            let id = Name.lower (sou base t.t_ident) in
            let pl = Name.lower (sou base t.t_place) in
            if pl = "" then if List.mem id allowed_titles then t :: l else l
            else if
              List.mem (id ^ "/" ^ pl) allowed_titles
              || List.mem (id ^ "/*") allowed_titles
            then t :: l
            else l)
          list []
      in
      match Lazy.force denied_titles with
      | [] -> list
      | denied_titles ->
          List.filter
            (fun t ->
              let id = Name.lower (sou base t.t_ident) in
              let pl = Name.lower (sou base t.t_place) in
              if
                List.mem (id ^ "/" ^ pl) denied_titles
                || List.mem ("*/" ^ pl) denied_titles
              then false
              else true)
            list)

(** Returns first name of person *)
let p_first_name base p = Mutil.nominative (sou base (get_first_name p))

(** Returns surname of person *)
let p_surname base p = Mutil.nominative (sou base (get_surname p))

(** Returns array of surnames of person's husbands.
    First element of a couple in the array is husband's surname,
    second - is a husband's surname aliases *)
let husbands base gp =
  let p = poi base gp.key_index in
  Array.map
    (fun ifam ->
      let fam = foi base ifam in
      let husband = poi base (get_father fam) in
      let husband_surname = get_surname husband in
      let husband_surnames_aliases = get_surnames_aliases husband in
      (husband_surname, husband_surnames_aliases))
    (get_family p)

(** Return person's father titles *)
let father_titles_places base p (nobtit : person -> title list) =
  match get_parents (poi base p.key_index) with
  | Some ifam ->
      let fam = foi base ifam in
      let fath = poi base (get_father fam) in
      nobtit fath
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  Futil.gen_person_misc_names (sou base) empty_string quest_string p.first_name
    p.surname p.public_name p.qualifiers p.aliases p.first_names_aliases
    p.surnames_aliases nobtit
    (if p.sex = Female then husbands base p else [||])
    (father_titles_places base p nobtit_fun)
  |> List.map Name.lower

(** [person_misc_names base p nobtit] computes various mix between all kind of names of a person's entry [p]
    from the database [base]. [nobtit] is used to return a title entries for passed in argument person. *)
let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit

(** Returns list of children ids for every family for giving person *)
let children_of_p base p =
  Array.fold_right
    (fun ifam -> Array.fold_right List.cons (get_children @@ foi base ifam))
    (get_family p) []
