open Def

include Gwdb_driver

(** [insert_person base per]
    Add a new person with the same properties as [per] in [base],
    returning the fresh new {!type:iper} for this person.
    [per] SHOULD be defined using [dummy_iper].
*)
let insert_person base p a u =
  let iper = Gwdb_driver.new_iper base in
  let p = { p with key_index = iper } in
  Gwdb_driver.insert_ascend base iper a ;
  Gwdb_driver.insert_union base iper u ;
  Gwdb_driver.insert_person base iper p ;
  iper

(** [insert_family base fam]
    Add a new family with the same properties as [fam] in [base],
    returning the fresh new {!type:ifam} for this family.
    [fam] SHOULD be defined using [dummy_ifam].
*)
let insert_family base f c d =
  let ifam = Gwdb_driver.new_ifam base in
  Gwdb_driver.insert_family base ifam f ;
  Gwdb_driver.insert_couple base ifam c ;
  Gwdb_driver.insert_descend base ifam d ;
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
  assert (ip <> dummy_iper && not @@ List.mem ip iexcl) ;
  let spouse c =
    let f = Adef.father c in
    if ip = f then Adef.mother c else f
  in
  let a = get_gen_ascend base ip in
  let ipers, ifams =
    match a.parents with
    | Some ifam ->
      Gwdb_driver.delete_ascend base ip ;
      let children =
        (get_gen_descend base ifam).children
        |> Mutil.array_except ip
      in
      Gwdb_driver.patch_descend base ifam { children } ;
      if children = [| ip |]
      then
        let c = get_gen_couple base ifam in
        let fath = Adef.father c in
        let moth = Adef.mother c in
        if is_empty_p base fath ~ifam && is_empty_p base moth ~ifam
        then [ fath ; moth ], [ ifam ]
        else [], []
      else [], []
    | None -> [], []
  in
  let del, ipers, ifams =
    let u = get_gen_union base ip in
    if u.family = [||] then (true, [], [])
    else
      Array.fold_left begin fun (del, ipers, ifams) ifam ->
        (* let f = get_family base ifam in *)
        let d = get_gen_descend base ifam in
        if Array.length d.children > 1 then (false, ipers, ifams)
        else begin
          let sp = spouse @@ get_gen_couple base ifam in
          if List.mem sp iexcl
          then (del, ipers, ifams)
          else if is_empty_p base sp ~ifam
          then (del, sp :: ipers, ifam :: ifams)
          else (false, ipers, ifams)
        end
      end (true, ipers, ifams) u.family
  in
  if del
  then Gwdb_driver.delete_person base ip
  else Gwdb_driver.patch_person base ip
      { (no_person ip) with first_name = quest_string
                          ; surname = quest_string } ;
  let iexcl = if del then ip :: iexcl else iexcl in
  let excl = iexcl, fexcl in
  let excl =
    List.fold_left begin fun excl ip ->
      delete_person excl base ip
    end excl ipers
  in
  List.fold_left begin fun excl ifam ->
    delete_family excl base ifam
  end excl ifams

and is_empty_p ?ifam base sp =
  (get_gen_ascend base sp).parents = None
  && (get_gen_union base sp).family = (match ifam with Some i -> [| i |] | None -> [||])
  && (get_gen_person base sp) =
     { (no_person sp) with first_name = quest_string
                         ; surname = quest_string }

and delete_family excl base ifam =
  let iexcl, fexcl = excl in
  assert (ifam <> dummy_ifam && not @@ List.mem ifam fexcl) ;
  let fam = foi base ifam in
  let fath = get_father fam in
  let moth = get_mother fam in
  let children = get_children fam in
  rm_union base ifam fath ;
  rm_union base ifam moth ;
  Array.iter (fun i -> patch_ascend base i no_ascend) children ;
  Gwdb_driver.delete_family base ifam ;
  Gwdb_driver.delete_couple base ifam ;
  Gwdb_driver.delete_descend base ifam ;
  let fexcl = ifam :: fexcl in
  let excl = iexcl, fexcl in
  let excl =
    if not (List.mem fath iexcl) && is_empty_p base fath
    then delete_person excl base fath
    else excl
  in
  let excl =
    if not (List.mem moth iexcl) && is_empty_p base moth
    then delete_person excl base moth
    else excl
  in
  Array.fold_left begin fun excl i ->
    if not (List.mem i iexcl) && is_empty_p base i
    then delete_person excl base i
    else excl
  end excl children

and rm_union base ifam iper =
  { family = (get_gen_union base iper).family
             |> Mutil.array_except ifam
  }
  |> patch_union base iper

let delete_person base iper = ignore @@ delete_person ([], []) base iper
let delete_family base ifam = ignore @@ delete_family ([], []) base ifam

(**/**)
(** Misc *)

let nobtit base allowed_titles denied_titles p =
  let list = get_titles p in
  match Lazy.force allowed_titles with
  | [] -> list
  | allowed_titles ->
    let list =
      List.fold_right
        (fun t l ->
           let id = Name.lower (sou base t.t_ident) in
           let pl = Name.lower (sou base t.t_place) in
           if pl = "" then
             if List.mem id allowed_titles then t :: l else l
           else if
             List.mem (id ^ "/" ^ pl) allowed_titles ||
             List.mem (id ^ "/*") allowed_titles
           then
             t :: l
           else l)
        list []
    in
    match Lazy.force denied_titles with
      [] -> list
    | denied_titles ->
      List.filter
        (fun t ->
           let id = Name.lower (sou base t.t_ident) in
           let pl = Name.lower (sou base t.t_place) in
           if List.mem (id ^ "/" ^ pl) denied_titles ||
              List.mem ("*/" ^ pl) denied_titles
           then
             false
           else true)
        list

let p_first_name base p = Mutil.nominative (sou base (get_first_name p))
let p_surname base p = Mutil.nominative (sou base (get_surname p))

let husbands base gp =
  let p = poi base gp.key_index in
  Array.map begin fun ifam ->
    let fam = foi base ifam in
    let husband = poi base (get_father fam) in
    let husband_surname = get_surname husband in
    let husband_surnames_aliases = get_surnames_aliases husband in
    husband_surname, husband_surnames_aliases
  end (get_family p)

let father_titles_places base p nobtit =
  match get_parents (poi base p.key_index) with
  | Some ifam ->
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    (nobtit fath)
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  Futil.gen_person_misc_names
    (sou base) empty_string quest_string
    p.first_name p.surname p.public_name p.qualifiers p.aliases
    p.first_names_aliases p.surnames_aliases
    nobtit
    (if p.sex = Female then husbands base p else [||])
    (father_titles_places base p nobtit_fun)
  |> List.map Name.lower

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit
