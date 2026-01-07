type patch =
  | Fix_NBDS of Gwdb.iper
  | Fix_AddedUnion of Gwdb.iper
  | Fix_AddedParents of Gwdb.iper
  | Fix_ParentDeleted of Gwdb.iper
  | Fix_AddedChild of Gwdb.ifam
  | Fix_RemovedUnion of Gwdb.iper * Gwdb.ifam
  | Fix_RemovedDuplicateUnion of Gwdb.iper * Gwdb.ifam
  | Fix_AddedRelatedFromPevent of Gwdb.iper * Gwdb.iper
  | Fix_AddedRelatedFromFevent of Gwdb.iper * Gwdb.iper
  | Fix_MarriageDivorce of Gwdb.ifam
  | Fix_MissingSpouse of Gwdb.ifam * Gwdb.iper
  | Fix_WrongString of
      Gwdb.ifam option * Gwdb.iper option * (Gwdb.istr * Gwdb.istr) option
  | Fix_UpdatedOcc of Gwdb.iper * int * int

let string_of_patch base =
  let string_of_p i = Gutil.designation base (Gwdb.poi base i) in
  let string_of_f i =
    let fam = Gwdb.foi base i in
    Printf.sprintf "[%s & %s]"
      (string_of_p @@ Gwdb.get_father fam)
      (string_of_p @@ Gwdb.get_mother fam)
  in
  function
  | Fix_NBDS ip -> Printf.sprintf "Fixed pevents for: %s" (string_of_p ip)
  | Fix_AddedUnion ip -> Printf.sprintf "Added union for: %s" (string_of_p ip)
  | Fix_AddedParents ip ->
      Printf.sprintf "Fixed missing parents for: %s" (string_of_p ip)
  | Fix_ParentDeleted ip ->
      Printf.sprintf "Deleted parents for: %s" (string_of_p ip)
  | Fix_AddedChild ifam ->
      Printf.sprintf "Added child in: %s" (string_of_f ifam)
  | Fix_RemovedUnion (ip, ifam) ->
      Printf.sprintf "Removing ifam %s from [%s] unions"
        (Gwdb.string_of_ifam ifam) (string_of_p ip)
  | Fix_RemovedDuplicateUnion (ip, ifam) ->
      Printf.sprintf "Removing duplicate ifam %s from [%s] unions"
        (Gwdb.string_of_ifam ifam) (string_of_p ip)
  | Fix_AddedRelatedFromPevent (ip, ip2) | Fix_AddedRelatedFromFevent (ip, ip2)
    ->
      Printf.sprintf "Added related %s to %s" (string_of_p ip2) (string_of_p ip)
  | Fix_MarriageDivorce ifam ->
      Printf.sprintf "Fixed marriage and/or divorce info of %s"
        (string_of_f ifam)
  | Fix_MissingSpouse (ifam, iper) ->
      Printf.sprintf "Fixed missing spouse (%s) in family %s" (string_of_p iper)
        (string_of_f ifam)
  | Fix_WrongString (ifam_opt, iper_opt, opt) ->
      Printf.sprintf "Fixed invalid string (%s): %s"
        (match ifam_opt with
        | Some i -> "ifam " ^ Gwdb.string_of_ifam i
        | None -> (
            match iper_opt with
            | Some i -> "iper " ^ Gwdb.string_of_iper i
            | None -> assert false))
        (match opt with
        | Some (i, i') ->
            Gwdb.string_of_istr i ^ " -> " ^ Gwdb.string_of_istr i'
        | None -> "Dtext")
  | Fix_UpdatedOcc (iper, oocc, nocc) ->
      Printf.sprintf "Uptated occ for %s: %d -> %d" (string_of_p iper) oocc nocc

let mk_pevent name date place note src =
  {
    Def.epers_name = name;
    epers_date = date;
    epers_place = place;
    epers_reason = Gwdb.empty_string;
    epers_note = note;
    epers_src = src;
    epers_witnesses = [||];
  }

let of_pevent e = (e.Def.epers_date, e.epers_place, e.epers_note, e.epers_src)

let find_pevent names pevents =
  List.find_opt (fun x -> List.mem x.Def.epers_name names) pevents

let fix_pevents ~report base pp =
  (* Should it use UpdateIndOk.reconstitute_from_pevents? *)
  (* TODO clean up *)
  let p = Gwdb.gen_person_of_person pp in
  let empty_bi =
    (Date.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string)
  in
  let empty_bp =
    (Date.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string)
  in
  let empty_de =
    (Date.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string)
  in
  let empty_bu =
    (Date.cdate_None, Gwdb.empty_string, Gwdb.empty_string, Gwdb.empty_string)
  in
  let pevents = p.pevents in
  let aux name date place note src empty pevents =
    match find_pevent [ name ] pevents with
    | None ->
        let pevents =
          if (date, place, note, src) <> empty then
            mk_pevent name date place note src :: pevents
          else pevents
        in
        ((date, place, note, src), pevents)
    | Some e ->
        let e' =
          {
            Def.epers_name = e.epers_name;
            epers_date =
              (if e.epers_date = Date.cdate_None then date else e.epers_date);
            epers_place =
              (if e.epers_place = Gwdb.empty_string then place
              else e.epers_place);
            epers_reason = e.epers_reason;
            epers_note =
              (if e.epers_note = Gwdb.empty_string then note else e.epers_note);
            epers_src =
              (if e.epers_src = Gwdb.empty_string then src else e.epers_src);
            epers_witnesses = e.epers_witnesses;
          }
        in
        (of_pevent e', Ext_list.replace e e' pevents)
  in
  let (birth, birth_place, birth_note, birth_src), pevents =
    aux Epers_Birth p.birth p.birth_place p.birth_note p.birth_src empty_bi
      pevents
  in
  let (baptism, baptism_place, baptism_note, baptism_src), pevents =
    aux Epers_Baptism p.baptism p.baptism_place p.baptism_note p.baptism_src
      empty_bp pevents
  in
  let (death, death_place, death_note, death_src), pevents =
    let death =
      match p.death with
      | Death (_, d) -> d
      | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead
        ->
          Date.cdate_None
    in
    aux Epers_Death death p.death_place p.death_note p.death_src empty_de
      pevents
  in
  let death =
    match p.death with
    | Death _ -> p.death
    | NotDead when death <> Date.cdate_None -> Death (Unspecified, death)
    | x -> x
  in
  let (burial, burial_place, burial_note, burial_src), pevents =
    match p.burial with
    | UnknownBurial -> (
        match find_pevent [ Epers_Burial; Epers_Cremation ] pevents with
        | None ->
            ( (Def.UnknownBurial, p.burial_place, p.burial_note, p.burial_src),
              pevents )
        | Some e ->
            let bu, bu_place, bu_note, bu_src = of_pevent e in
            ( ( (match e.epers_name with
                | Epers_Burial -> Buried bu
                | Epers_Cremation -> Cremated bu
                | _ -> assert false),
                bu_place,
                bu_note,
                bu_src ),
              pevents ))
    | Buried d ->
        let (d, bu_place, bu_note, bu_src), pevents =
          aux Epers_Burial d p.burial_place p.burial_note p.burial_src empty_bu
            pevents
        in
        ((Buried d, bu_place, bu_note, bu_src), pevents)
    | Cremated d ->
        let (d, bu_place, bu_note, bu_src), pevents =
          aux Epers_Cremation d p.burial_place p.burial_note p.burial_src
            empty_bu pevents
        in
        ((Cremated d, bu_place, bu_note, bu_src), pevents)
  in
  let p' =
    {
      p with
      birth;
      birth_place;
      birth_note;
      birth_src;
      baptism;
      baptism_place;
      baptism_note;
      baptism_src;
      death;
      death_place;
      death_note;
      death_src;
      burial;
      burial_place;
      burial_note;
      burial_src;
      pevents;
    }
  in
  let person_changed = p <> p' in
  let () =
    if person_changed then (
      Gwdb.patch_person base p.key_index p';
      Option.iter (fun fn -> fn (Fix_NBDS p.key_index)) report)
  in
  person_changed

type person_fix =
  report:(patch -> unit) option -> base:Gwdb.base -> person:Gwdb.person -> bool

type family_fix =
  report:(patch -> unit) option -> base:Gwdb.base -> family:Gwdb.family -> bool

let fix_nbds ~report ~base ~person = fix_pevents ~report base person

let fix_family_parents ~report ~base ~family =
  let ifam = Gwdb.get_ifam family in
  let parents = Gwdb.get_parent_array family in
  Array.fold_left
    (fun change ip ->
      let unions = Gwdb.get_family (Gwdb.poi base ip) in
      if not @@ Array.mem ifam unions then (
        Gwdb.patch_union base ip { family = Array.append unions [| ifam |] };
        Option.iter (fun fn -> fn (Fix_AddedUnion ip)) report;
        true)
      else change)
    false parents

let fix_family_children ~report ~base ~family =
  let ifam = Gwdb.get_ifam family in
  let children = Gwdb.get_children family in
  Array.fold_left
    (fun change child_iper ->
      let child = Gwdb.poi base child_iper in
      let parents = Gwdb.get_parents child in
      if parents = Some Gwdb.dummy_ifam || parents = None then (
        let gen_ascend =
          { Def.parents = Some ifam; consang = Gwdb.get_consang child }
        in
        Gwdb.patch_ascend base child_iper gen_ascend;
        Option.iter (fun fn -> fn (Fix_AddedParents child_iper)) report;
        true)
      else change)
    false children

let fix_person_parents ~report ~base ~person =
  let parents = Gwdb.get_parents person in
  match parents with
  | Some parents ->
      let family = Gwdb.foi base parents in
      let ifam = Gwdb.get_ifam family in
      if ifam = Gwdb.dummy_ifam then (
        let gen_ascend = { Def.parents = None; consang = Adef.no_consang } in
        let iper = Gwdb.get_iper person in
        Gwdb.patch_ascend base iper gen_ascend;
        Option.iter (fun fn -> fn (Fix_ParentDeleted iper)) report;
        true)
      else
        let children = Gwdb.get_children family in
        let iper = Gwdb.get_iper person in
        if not (Array.mem iper children) then (
          let children = Array.append children [| iper |] in
          Gwdb.patch_descend base ifam { children };
          Option.iter (fun fn -> fn (Fix_AddedChild ifam)) report;
          true)
        else false
  | None -> false

let is_a_parent iper family = Array.mem iper (Gwdb.get_parent_array family)

(* This fix removes the duplicated unions and the now invalid unions (the person
   is not part of the union anymore). We want to keep the original order of the remaining
   unions.*)
let fix_person_unions ~report ~base ~person =
  let iper = Gwdb.get_iper person in
  let ifams = Gwdb.get_family person in
  let change, ifams, _ifam_set =
    Array.fold_right
      (fun ifam (change, ifams, ifam_set) ->
        if Gwdb.IfamSet.mem ifam ifam_set then (
          Option.iter
            (fun fn -> fn (Fix_RemovedDuplicateUnion (iper, ifam)))
            report;
          (true, ifams, ifam_set))
        else if not (is_a_parent iper (Gwdb.foi base ifam)) then (
          Option.iter (fun fn -> fn (Fix_RemovedUnion (iper, ifam))) report;
          (true, ifams, ifam_set))
        else (change, ifam :: ifams, Gwdb.IfamSet.add ifam ifam_set))
      ifams
      (false, [], Gwdb.IfamSet.empty)
  in
  if change then Gwdb.patch_union base iper { family = Array.of_list ifams };
  change

let fix_related report base patch_cons iper change iper_wit =
  let witness = Gwdb.poi base iper_wit in
  let witness_related = Gwdb.get_related witness in
  if not (List.memq iper witness_related) then (
    Option.iter (fun fn -> fn (patch_cons iper_wit iper)) report;
    let gen_person =
      {
        (Gwdb.gen_person_of_person witness) with
        related = iper :: witness_related;
      }
    in
    Gwdb.patch_person base iper_wit gen_person;
    true)
  else change

let added_related_from_pevent iper_wit iper =
  Fix_AddedRelatedFromPevent (iper_wit, iper)

let fix_person_events_witnesses ~report ~base ~person =
  let iper = Gwdb.get_iper person in
  List.fold_left
    (fun change evt ->
      let witnesses = Array.map fst (Gwdb.get_pevent_witnesses evt) in
      Array.fold_left
        (fix_related report base added_related_from_pevent iper)
        change witnesses)
    false (Gwdb.get_pevents person)

let added_related_from_fevent iper_wit iper =
  Fix_AddedRelatedFromFevent (iper_wit, iper)

let fix_family_events_witnesses ~report ~base ~family =
  let iper = Gwdb.get_father family in
  List.fold_left
    (fun change evt ->
      let witnesses = Array.map fst (Gwdb.get_fevent_witnesses evt) in
      Array.fold_left
        (fix_related report base added_related_from_fevent iper)
        change witnesses)
    false (Gwdb.get_fevents family)

let fix_family_divorce ~report ~base ~family =
  let fevents =
    List.map Gwdb.gen_fevent_of_fam_event (Gwdb.get_fevents family)
  in
  let relation' = Gwdb.get_relation family in
  let marriage' = Gwdb.get_marriage family in
  let marriage_place' = Gwdb.get_marriage_place family in
  let marriage_note' = Gwdb.get_marriage_note family in
  let marriage_src' = Gwdb.get_marriage_src family in
  let divorce' = Gwdb.get_divorce family in
  let marriage_data' =
    (relation', marriage', marriage_place', marriage_note', marriage_src')
  in
  let ( ((relation, marriage, marriage_place, marriage_note, marriage_src) as
        marriage_data),
        divorce,
        _ ) =
    UpdateFamOk.reconstitute_from_fevents false
      (Gwdb.insert_string base "")
      fevents
  in
  if marriage_data <> marriage_data' || divorce <> divorce' then (
    let gen_family =
      {
        (Gwdb.gen_family_of_family family) with
        relation;
        marriage;
        marriage_place;
        marriage_note;
        marriage_src;
        divorce;
      }
    in
    let ifam = Gwdb.get_ifam family in
    Gwdb.patch_family base (Gwdb.get_ifam family) gen_family;
    Option.iter (fun fn -> fn (Fix_MarriageDivorce ifam)) report;
    true)
  else false

let fix_family_spouses ~report ~base ~family =
  let fix_spouse_union iper =
    let person = Gwdb.poi base iper in
    if Gwdb.get_iper person = Gwdb.dummy_iper then (
      let ifam = Gwdb.get_ifam family in
      Gwdb.patch_union base iper { family = [| ifam |] };
      Gwdb.patch_person base iper
        { (Gwdb.gen_person_of_person person) with key_index = iper };
      Option.iter (fun fn -> fn (Fix_MissingSpouse (ifam, iper))) report;
      true)
    else false
  in
  let change_fath = fix_spouse_union (Gwdb.get_father family) in
  let change_moth = fix_spouse_union (Gwdb.get_mother family) in
  change_fath || change_moth

let fix_map_utf8_date ~report = function
  | Date.Dtext t ->
      let t' = Utf8.normalize t in
      if t <> t' then report ();
      Date.Dtext t'
  | d -> d

let fix_map_str ~report ~base ?format istr =
  let s = Gwdb.sou base istr in
  let s' =
    let clean =
      match format with
      | Some `Html -> Fun.id
      | None | Some (`Plain_text | `First_name | `Surname) -> Html.text_content
    in
    Utf8.normalize @@ clean s
  in
  let istr' =
    let s =
      match format with
      | None | Some (`Html | `Plain_text) -> s'
      | Some (`First_name | `Surname) -> if s' = "" then "x" else s'
    in
    let format =
      match format with
      | (None | Some (`Html | `Plain_text)) as format -> format
      | Some (`First_name | `Surname) -> Some `Plain_text
    in
    Gwdb.insert_string base ?format s
  in
  if istr <> istr' then report istr istr';
  istr'

let fix_person_strings ~report ~base ~person =
  let iper = Gwdb.get_iper person in
  let change = ref false in
  let report_date () =
    Option.iter (fun fn -> fn (Fix_WrongString (None, Some iper, None))) report;
    change := true
  in
  let report_str istr istr' =
    Option.iter
      (fun fn -> fn (Fix_WrongString (None, Some iper, Some (istr, istr'))))
      report;
    change := true
  in
  let fix_map_date = fix_map_utf8_date ~report:report_date in
  let fix_map_str ?format = fix_map_str ~report:report_str ~base ?format in
  let gen_pers = Gwdb.gen_person_of_person person in
  let gen_pers' =
    Futil.map_person_ps ~fd:fix_map_date
      ~f_first_name:(fix_map_str ~format:`First_name)
      ~f_surname:(fix_map_str ~format:`Surname)
      Fun.id fix_map_str gen_pers
  in
  if gen_pers' <> gen_pers then (
    Gwdb.patch_person base iper gen_pers';
    true)
  else !change

let fix_family_strings ~report ~base ~family =
  let ifam = Gwdb.get_ifam family in
  let change = ref false in
  let report_date () =
    Option.iter (fun fn -> fn (Fix_WrongString (Some ifam, None, None))) report;
    change := true
  in
  let report_str istr istr' =
    Option.iter
      (fun fn -> fn (Fix_WrongString (Some ifam, None, Some (istr, istr'))))
      report;
    change := true
  in
  let fix_map_date = fix_map_utf8_date ~report:report_date in
  let fix_map_str ?format = fix_map_str ~report:report_str ~base ?format in
  let gen_fam = Gwdb.gen_family_of_family family in
  let gen_fam' =
    Futil.map_family_ps ~fd:fix_map_date Fun.id Fun.id fix_map_str gen_fam
  in
  if gen_fam' <> gen_fam then (
    Gwdb.patch_family base ifam gen_fam';
    true)
  else !change

let find_free_occ occ_set =
  let occ = ref 0 in
  let rec loop () =
    if Ext_int.Set.mem !occ occ_set then (
      incr occ;
      loop ())
    else
      let found_occ = !occ in
      incr occ;
      found_occ
  in
  loop

let fix_person_key base =
  let skip = Gwdb.iper_marker (Gwdb.ipers base) false in
  fun ~report ~base ~person : bool ->
    let iper = Gwdb.get_iper person in
    if not (Gwdb.Marker.get skip iper) then
      let first_name = Gwdb.p_first_name base person in
      let surname = Gwdb.p_surname base person in
      if first_name <> "?" && surname <> "?" then (
        let homonyms = Gutil.homonyms ~base ~first_name ~surname in
        let ipers = List.sort Gwdb.compare_iper homonyms in
        List.iter (fun iper -> Gwdb.Marker.set skip iper true) ipers;
        let occ_set =
          List.fold_left
            (fun occ_set iper ->
              let p = Gwdb.poi base iper in
              Ext_int.Set.add (Gwdb.get_occ p) occ_set)
            Ext_int.Set.empty ipers
        in
        let first_free_occ = find_free_occ occ_set in
        let change, remaining_occ_set =
          List.fold_left
            (fun (change, remaining_occ_set) iper ->
              let p = Gwdb.poi base iper in
              let occ = Gwdb.get_occ p in
              let set' = Ext_int.Set.remove occ remaining_occ_set in
              if set' == remaining_occ_set then (
                let occ' = first_free_occ () in
                Gwdb.patch_person base iper
                  { (Gwdb.gen_person_of_person p) with occ = occ' };
                Option.iter
                  (fun fn -> fn (Fix_UpdatedOcc (iper, occ, occ')))
                  report;
                (true, remaining_occ_set))
              else (change, set'))
            (false, occ_set) ipers
        in
        assert (Ext_int.Set.is_empty remaining_occ_set);
        change)
      else false
    else false

let fix_invalid_occurrence_number ~report ~base ~person =
  let current_occurrence_number = Gwdb.get_occ person in
  let is_invalid_occurrence_number =
    not @@ Occurrence_number.is_valid current_occurrence_number
  in
  let () =
    if is_invalid_occurrence_number then (
      let new_occurrence_number =
        Gutil.find_free_occ base
          (Gwdb.p_first_name base person)
          (Gwdb.p_surname base person)
      in
      let person_id = Gwdb.get_iper person in
      Gwdb.patch_person base person_id
        { (Gwdb.gen_person_of_person person) with occ = new_occurrence_number };
      Option.iter
        (fun report ->
          report
            (Fix_UpdatedOcc
               (person_id, current_occurrence_number, new_occurrence_number)))
        report)
  in
  is_invalid_occurrence_number

let perform_fixes ~(report : (patch -> unit) option) ~progress ~base
    ~(person_fixes : person_fix list) ~(family_fixes : family_fix list) =
  if person_fixes = [] && family_fixes = [] then 0
  else
    let n_persons = Gwdb.nb_of_persons base in
    let n_families = Gwdb.nb_of_families base in
    let end_progress = if person_fixes <> [] then n_persons else 0 in
    let fstart_progress, end_progress =
      if family_fixes <> [] then (end_progress, end_progress + n_families)
      else (0, end_progress)
    in
    let nb_fixes = ref 0 in
    (if person_fixes <> [] then
     let persons = Gwdb.persons base in
     Gwdb.Collection.iteri
       (fun i person ->
         progress i end_progress;
         List.iter
           (fun fix -> if fix ~report ~base ~person then incr nb_fixes)
           person_fixes)
       persons);
    (if family_fixes <> [] then
     let families = Gwdb.families base in
     Gwdb.Collection.iteri
       (fun i family ->
         progress (fstart_progress + i) end_progress;
         List.iter
           (fun fix -> if fix ~report ~base ~family then incr nb_fixes)
           family_fixes)
       families);
    !nb_fixes
