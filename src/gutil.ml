(* $Id: gutil.ml,v 3.0 1999-10-29 10:31:12 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value poi base i = base.data.persons.get (Adef.int_of_iper i);
value aoi base i = base.data.ascends.get (Adef.int_of_iper i);
value foi base i = base.data.families.get (Adef.int_of_ifam i);
value coi base i = base.data.couples.get (Adef.int_of_ifam i);
value sou base i = base.data.strings.get (Adef.int_of_istr i);

value rindex s c =
  pos (String.length s - 1) where rec pos i =
    if i < 0 then None else if s.[i] = c then Some i else pos (i - 1)
;

value lindex s c =
  pos 0 where rec pos i =
    if i == String.length s then None
    else if s.[i] == c then Some i else pos (i + 1)
;

value array_memq x a =
  loop 0 where rec loop i =
    if i == Array.length a then False
    else if x == a.(i) then True
    else loop (i + 1)
;

value string_sub s i len =
  let i = min (String.length s) (max 0 i) in
  let len = min (String.length s - i) (max 0 len) in
  String.sub s i len
;

value decline_word case s ibeg iend =
  let i =
    loop ibeg where rec loop i =
      if i + 3 >= iend then ibeg
      else if s.[i] == ':' && s.[i+1] == case && s.[i+2] == ':' then i + 3
      else loop (i + 1)
  in
  let j =
    loop i where rec loop i =
      if i + 3 >= iend then iend
      else if s.[i] == ':' && s.[i+2] == ':' then i
      else loop (i + 1)
  in
  if s.[i] == '+' then
    let k =
      loop ibeg where rec loop i =
        if i == iend then i
        else if s.[i] == ':' then i
        else loop (i + 1)
    in
    let i = i + 1 in
    string_sub s ibeg (k - ibeg) ^ string_sub s i (j - i)
  else if s.[i] == '-' then
    let k =
      loop ibeg where rec loop i =
        if i == iend then i
        else if s.[i] == ':' then i
        else loop (i + 1)
    in
    let (i, cnt) =
      loop (i + 1) 1 where rec loop i cnt =
        if i < iend && s.[i] == '-' then loop (i + 1) (cnt + 1)
        else (i, cnt)
    in
    string_sub s ibeg (k - ibeg - cnt) ^ string_sub s i (j - i)
  else string_sub s i (j - i)
;

value decline case s =
  loop 0 0 where rec loop ibeg i =
    if i == String.length s then
      if i == ibeg then "" else decline_word case s ibeg i
    else if s.[i] == ' ' then
      decline_word case s ibeg i ^ " " ^ loop (i + 1) (i + 1)
    else if s.[i] == '<' then
      decline_word case s ibeg i ^ "<" ^ loop (i + 1) (i + 1)
    else if s.[i] == '>' then
      String.sub s ibeg (i + 1 - ibeg) ^ loop (i + 1) (i + 1)
    else loop ibeg (i + 1)
;

value nominative s =
  match rindex s ':' with
  [ Some _ -> decline 'n' s
  | _ -> s ]
;

value p_first_name base p = nominative (sou base p.first_name);
value p_surname base p = nominative (sou base p.surname);

value leap_year a =
  if a mod 100 == 0 then a / 100 mod 4 == 0 else a mod 4 == 0
;

value nb_jours_dans_mois =
  let tb = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  fun m a ->
    if m == 2 && leap_year a then 29
    else if m >= 1 && m <= 12 then tb.(m - 1)
    else 31
;

value common_prec p1 p2 =
 if p1 = p2 then p1
  else
    match (p1, p2) with
    [ (Sure, _) -> p2
    | (_, Sure) -> p1
    | _ -> Maybe ]
;

value temps_ecoule d1 d2 =
  let prec = common_prec d1.prec d2.prec in
  match d1 with
  [ {day = 0; month = 0; year = a1} ->
      {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in
          {day = 0; month = mois; year = annee; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let r = 0 in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in
          {day = 0; month = mois; year = annee; prec = prec; delta = 0} ]
  | {day = j1; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in
          {day = 0; month = mois; year = annee; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let (jour, r) =
            if j1 <= j2 then (j2 - j1, 0)
            else (j2 - j1 + nb_jours_dans_mois m1 a1, 1)
          in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in
          {day = jour; month = mois; year = annee; prec = prec; delta = 0} ] ]
;

value annee d = d.year;

value strictement_avant_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = temps_ecoule d2 d1 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d1.prec = d2.prec then False
  else if d1.prec = Before && d2.prec = After then True
  else False
;

value strictement_avant d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictement_avant_dmy d1 d2
  | _ -> False ]
;

value strictement_apres_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = temps_ecoule d1 d2 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d2.prec = d1.prec then False
  else if d2.prec = Before && d1.prec = After then True
  else False
;

value strictement_apres d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictement_apres_dmy d1 d2
  | _ -> False ]
;

value denomination base p =
  let prenom = p_first_name base p in
  let nom = p_surname base p in
  prenom ^
   (if p.occ == 0 || prenom = "?" || nom = "?" then ""
    else "." ^ string_of_int p.occ) ^
   " " ^ nom
;

value saints = ["saint"; "sainte"];

value surnames_pieces surname =
  let surname = Name.lower surname in
  let flush i0 i1 =
    if i1 > i0 then [String.sub surname i0 (i1 - i0)] else []
  in
  let rec loop i0 iw i =
    if i == String.length surname then
      if i0 == 0 then [] else if i > i0 + 3 then flush i0 i else []
    else if surname.[i] == ' ' then
      if i > iw + 3 then
        let w = String.sub surname iw (i - iw) in
        if List.mem w saints then loop i0 (i + 1) (i + 1)
        else flush i0 i @ loop (i + 1) (i + 1) (i + 1)
      else loop i0 (i + 1) (i + 1)
    else loop i0 iw (i + 1)
  in
  loop 0 0 0
;

value person_misc_names base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then [] else
  let public_names =
    let titles_names =
      let tnl = ref [] in
      do List.iter
           (fun t ->
              match t.t_name with
              [ Tmain | Tnone -> ()
              | Tname x -> tnl.val := [x :: tnl.val ] ])
           p.titles;
      return tnl.val
    in
    if sou base p.public_name = "" then titles_names
    else [p.public_name :: titles_names]
  in
  let first_names =
    [first_name ::
     List.map (sou base) (p.first_names_aliases @ public_names)]
  in
  let surnames =
    [surname ::
       surnames_pieces surname @
       List.map (sou base) (p.surnames_aliases @ p.nick_names)]
  in
  let surnames =
    if p.sex == Female then
      List.fold_left
        (fun list ifam ->
           let cpl = coi base ifam in
           let husband = poi base cpl.father in
           let husband_surname = p_surname base husband in
           let husband_surnames_aliases =
             List.map (sou base) husband.surnames_aliases
           in
           if p_surname base husband = "?" then
             husband_surnames_aliases @ list
           else
             [husband_surname ::
                surnames_pieces husband_surname @ husband_surnames_aliases @
                list])
        surnames (Array.to_list p.family)
    else surnames
  in
  let list = [] in
  let list =
    List.fold_left (fun list s -> [sou base s :: list]) list public_names
  in
  let list =
    List.fold_left
       (fun list f ->
          List.fold_left (fun list s -> [f ^ " " ^ s :: list]) list surnames)
    list first_names
  in
  let list =
    let first_names =
      [first_name :: List.map (sou base) p.first_names_aliases]
    in
    List.fold_left
      (fun list t ->
         let s = sou base t.t_place in
         if s = "" then list
         else
           let first_names =
             match t.t_name with
             [ Tname f -> [sou base f :: first_names]
             | _ ->
                 let f = sou base p.public_name in
                 if f = "" then first_names
                 else [f :: first_names] ]
           in
           List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
             first_names)
      list p.titles
  in
  let list =
    match (aoi base p.cle_index).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        let fath = poi base cpl.father in
        let first_names =
          [first_name :: List.map (sou base) p.first_names_aliases]
        in
        List.fold_left
          (fun list t ->
             let s = sou base t.t_place in
             if s = "" then list
             else
               List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
                 first_names)
          list fath.titles
    | _ -> list ]
  in
  let list =
    List.fold_left (fun list s -> [sou base s :: list]) list p.aliases
  in
  let fn = Name.lower (first_name ^ " " ^ surname) in
  List.fold_left
    (fun list s ->
       let s = Name.lower s in
       if s = fn || List.mem s list then list else [s :: list])
    [] list
;

value person_ht_add base s ip = base.func.patch_name s ip;

value person_is_key base p k =
  let k = Name.crush_lower k in
  if k = Name.crush_lower (p_first_name base p ^ " " ^ p_surname base p)
  then True
  else if
    List.exists (fun x -> k = Name.crush_lower x) (person_misc_names base p)
  then True
  else False
;

value person_ht_find_unique base first_name surname occ =
  if first_name = "?" || surname = "?" then raise Not_found
  else
    let first_name = nominative first_name in
    let surname = nominative surname in
    let ipl = base.func.persons_of_name (first_name ^ " " ^ surname) in
    let first_name = Name.strip_lower first_name in
    let surname = Name.strip_lower surname in
    find ipl where rec find =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if occ == p.occ
          && first_name = Name.strip_lower (p_first_name base p)
          && surname = Name.strip_lower (p_surname base p)
          then p.cle_index
          else find ipl
      | _ -> raise Not_found ]
;

value find_num s i =
  loop i i where rec loop start i =
    if i == String.length s then None
    else
      match s.[i] with
      [ '0'..'9' -> loop start (i + 1)
      | c ->
          if i == start then
            if c = ' ' then loop (start + 1) (start + 1) else None
          else Some (int_of_string (String.sub s start (i - start)), i) ]
;

value get_unique base s =
  match lindex s '.' with
  [ Some i ->
      match find_num s (i + 1) with
      [ Some (occ, j) ->
          let first_name = String.sub s 0 i in
          let surname = String.sub s j (String.length s - j) in
          try Some (person_ht_find_unique base first_name surname occ) with
          [ Not_found -> None ]
      | None -> None ]
  | None -> None ]
;

value person_ht_find_all base s =
  match get_unique base s with
  [ Some p -> [p]
  | _ ->
      let ipl = base.func.persons_of_name s in
      select ipl where rec select =
        fun
        [ [ip :: ipl] ->
            if person_is_key base (poi base ip) s then
              let ipl = select ipl in
              if List.mem ip ipl then ipl else [ip :: ipl]
            else select ipl
        | [] -> [] ] ]
;

(* check base *)

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;
type base_error = error person;

type warning 'person =
  [ BirthAfterDeath of 'person
  | ChangedOrderOfChildren of family and array iper
  | ChildrenNotInOrder of family and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and Def.dmy
  | TitleDatesError of 'person and title
  | YoungForMarriage of 'person and Def.dmy ]
;
type base_warning = warning person;

type visit = [ NotVisited | BeingVisited | Visited ];

value check_noloop base error =
  let tab = Array.create (base.data.persons.len) NotVisited in
  let rec noloop i =
    match tab.(i) with
    [ NotVisited ->
        do match (base.data.ascends.get i).parents with
           [ Some fam ->
               let fath = (coi base fam).father in
               let moth = (coi base fam).mother in
               do tab.(i) := BeingVisited;
                  noloop (Adef.int_of_iper fath);
                  noloop (Adef.int_of_iper moth);
               return ()
           | None -> () ];
           tab.(i) := Visited;
        return ()
    | BeingVisited -> error (OwnAncestor (base.data.persons.get i))
    | Visited -> () ]
  in
  for i = 0 to base.data.persons.len - 1 do
    match tab.(i) with
    [ NotVisited -> noloop i
    | BeingVisited -> failwith "check_noloop algorithm error"
    | Visited -> () ];
  done
;

value check_noloop_for_person_list base error ipl =
  let tab = Array.create (base.data.persons.len) NotVisited in
  let rec noloop ip =
    let i = Adef.int_of_iper ip in
    match tab.(i) with
    [ NotVisited ->
        do match (aoi base ip).parents with
           [ Some ifam ->
               let cpl = coi base ifam in
               do tab.(i) := BeingVisited;
                  noloop cpl.father;
                  noloop cpl.mother;
               return ()
           | None -> () ];
           tab.(i) := Visited;
        return ()
    | BeingVisited -> error (OwnAncestor (poi base ip))
    | Visited -> () ]
  in
  List.iter noloop ipl
;

value date_of_death =
  fun
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value child_born_after_his_parent base error warning x iparent =
  let parent = poi base iparent in
  match
    (Adef.od_of_codate parent.birth, Adef.od_of_codate x.birth,
     date_of_death x.death)
  with
  [ (Some (Dgreg g1 _ as d1), Some (Dgreg g2 _ as d2), _) ->
      if strictement_apres d1 d2 then
        warning (ParentBornAfterChild parent x)
      else
        let a = temps_ecoule g1 g2 in
        if annee a < 11 then warning (ParentTooYoung parent a) else ()
  | (Some (Dgreg g1 _ as d1), _, Some (Dgreg g2 _ as d2)) ->
      if strictement_apres d1 d2 then
        warning (ParentBornAfterChild parent x)
      else
        let a = temps_ecoule g1 g2 in
        if annee a < 11 then warning (ParentTooYoung parent a) else ()
  | _ -> () ]
;

value born_after_his_elder_sibling base error warning x np fam =
  match (np, Adef.od_of_codate x.birth, x.death) with
  [ (None, _, _) -> ()
  | (Some (elder, d1), Some d2, _) ->
      if strictement_apres d1 d2 then warning (ChildrenNotInOrder fam elder x)
      else ()
  | (Some (elder, d1), _, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictement_apres d1 d2 then warning (ChildrenNotInOrder fam elder x)
      else ()
  | _ -> () ]
;

value child_born_before_mother_death base warning x imoth =
  let mother = poi base imoth in
  match (Adef.od_of_codate x.birth, mother.death) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictement_apres d1 d2 then
        warning (MotherDeadAfterChildBirth mother x)
      else ()
  | _ -> () ]
;

value possible_father base warning x ifath =
  let father = poi base ifath in
  match (Adef.od_of_codate x.birth, date_of_death father.death) with
  [ (Some (Dgreg {prec = Before} _), _)
  | (_, Some (Dgreg {prec = After} _)) -> ()
  | (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
      let a2 =
        match d2 with
        [ {prec = YearInt a2} -> a2
        | {prec = OrYear a2} -> a2
        | {year = a} -> a ]
      in
      if annee d1 > a2 + 1 then
        warning (DeadTooEarlyToBeFather father x)
      else ()
  | _ -> () ]
;

value birth_before_death base warning p =
  match (Adef.od_of_codate p.birth, p.death) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictement_apres d1 d2 then warning (BirthAfterDeath p)
      else ()
  | _ -> () ]
;

value titles_while_living base warning p t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do match (t_date_start, t_date_end) with
     [ (Some d1, Some d2) ->
         if strictement_apres d1 d2 then warning (TitleDatesError p t)
         else ()
     | _ -> () ];
     match Adef.od_of_codate p.birth with
     [ Some d1 ->
         do match t_date_start with
            [ Some d ->
                if strictement_apres d1 d then
                  warning (TitleDatesError p t)
                else ()
            | None -> () ];
            match t_date_end with
            [ Some d ->
                if strictement_apres d1 d then
                  warning (TitleDatesError p t)
                else ()
            | None -> () ];
         return ()
     | _ -> () ];
     match p.death with
     [ Death _ d1 ->
         let d1 = Adef.date_of_cdate d1 in
         do match t_date_start with
            [ Some d ->
                if strictement_apres d d1 then
                  warning (TitleDatesError p t)
                else ()
            | None -> () ];
            match t_date_end with
            [ Some d ->
                if strictement_apres d d1 then
                  warning (TitleDatesError p t)
                else ()
            | None -> () ];
         return ()
     | _ -> () ];
  return ()
;

value check_normal_marriage_date_for_someone base error warning fam ip =
  let p = poi base ip in
  match Adef.od_of_codate fam.marriage with
  [ Some d2 ->
      do match Adef.od_of_codate p.birth with
         [ Some d1 ->
             if strictement_avant d2 d1 then
               warning (MarriageDateBeforeBirth p)
(*
             else if
               annee d2 > 1850 && annee (temps_ecoule d1 d2) < 13 then
               warning (YoungForMarriage p (temps_ecoule d1 d2))
*)
             else ()
         | _ -> () ];
         match p.death with
         [ Death _ d3 ->
             let d3 = Adef.date_of_cdate d3 in
             if strictement_apres d2 d3 then
               warning (MarriageDateAfterDeath p)
             else ()
         | _ -> () ];
      return ()
  | None -> () ]
;

value check_normal_marriage_date base error warning fam =
  let cpl = coi base fam.fam_index in
  do check_normal_marriage_date_for_someone base error warning fam cpl.father;
     check_normal_marriage_date_for_someone base error warning fam cpl.mother;
  return ()
;

value sort_children base warning fam =
  let before = ref None in
  let a = fam.children in
  do for i = 1 to Array.length a - 1 do
       loop (i-1) where rec loop j =
         if j >= 0 then
           let p1 = poi base a.(j) in
           let p2 = poi base a.(j+1) in
           match (Adef.od_of_codate p1.birth, Adef.od_of_codate p2.birth) with
           [ (Some d1, Some d2) ->
               if strictement_avant d2 d1 then
                 let ip = a.(j+1) in
                 do match before.val with
                    [ Some _ -> ()
                    | None -> before.val := Some (Array.copy a) ];
                    a.(j+1) := a.(j);
                    a.(j) := ip;
                 return loop (j-1)
               else ()
           | _ -> () ]
         else ();
     done;
     match before.val with
     [ None -> ()
     | Some a -> warning (ChangedOrderOfChildren fam a) ];
  return ()
;

value check_family base error warning fam =
  let cpl = coi base fam.fam_index in
  do match (poi base cpl.father).sex with
     [ Male -> ()
     | _ -> error (BadSexOfMarriedPerson (poi base cpl.father)) ];
     match (poi base cpl.mother).sex with
     [ Female -> ()
     | _ -> error (BadSexOfMarriedPerson (poi base cpl.mother)) ];
     check_normal_marriage_date base error warning fam;
     sort_children base warning fam;
     let _ =
       List.fold_left
         (fun np child ->
            let child = poi base child in
            do born_after_his_elder_sibling base error warning child np fam;
               child_born_after_his_parent base error warning child cpl.father;
               child_born_after_his_parent base error warning child cpl.mother;
               child_born_before_mother_death base warning child cpl.mother;
               possible_father base warning child cpl.father;
            return
            match Adef.od_of_codate child.birth with
            [ Some d -> Some (child, d)
            | _ -> np ])
         None (Array.to_list fam.children)
     in
     ();
  return ()
;

value check_person base error warning p =
  do birth_before_death base warning p;
     List.iter (titles_while_living base warning p) p.titles;
  return ()
;

value is_deleted_family fam = fam.fam_index = Adef.ifam_of_int (-1);

value check_base base error warning =
  do for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       check_person base error warning p;
     done;
     for i = 0 to base.data.families.len - 1 do
       let fam = base.data.families.get i in
       if is_deleted_family fam then ()
       else check_family base error warning fam;
     done;
     check_noloop base error;
  return ()
;

value strip_controls_m s =
  let len =
    loop 0 0 where rec loop i len =
      if i == String.length s then len
      else if s.[i] == '\r' then loop (i + 1) len
      else loop (i + 1) (len + 1)
  in
  if len == String.length s then s
  else
    let s' = String.create len in
    loop 0 0 where rec loop i j =
      if j == len then s'
      else if s.[i] == '\r' then loop (i + 1) j
      else do s'.[j] := s.[i]; return loop (i + 1) (j + 1)
   
;

value strip_spaces str =
  let start = loop 0
    where rec loop i =
      if i == String.length str then i
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
        | _ -> i ]
  in
  let stop = loop (String.length str - 1)
    where rec loop i =
      if i == -1 then i + 1
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  if start == 0 && stop == String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)
;

value initiale n =
  boucle 0 where rec boucle i =
    if i == String.length n then 0
    else
      match n.[i] with
      [ 'A'..'Z' | 'À'..'Ý' -> i
      | _ -> boucle (succ i) ]
;

value valeur_alphabetique =
  let tab = Array.create 256 0 in
  do for i = 0 to 255 do tab.(i) := 10 * i; done;
     tab.(Char.code 'à') := tab.(Char.code 'a') + 1;
     tab.(Char.code 'á') := tab.(Char.code 'a') + 2;
     tab.(Char.code 'â') := tab.(Char.code 'a') + 3;
     tab.(Char.code 'è') := tab.(Char.code 'e') + 1;
     tab.(Char.code 'é') := tab.(Char.code 'e') + 2;
     tab.(Char.code 'ê') := tab.(Char.code 'e') + 3;
     tab.(Char.code 'ë') := tab.(Char.code 'e') + 4;
     tab.(Char.code 'ô') := tab.(Char.code 'o') + 1;
     tab.(Char.code 'Á') := tab.(Char.code 'A') + 2;
     tab.(Char.code 'Æ') := tab.(Char.code 'A') + 5;
     tab.(Char.code 'È') := tab.(Char.code 'E') + 1;
     tab.(Char.code 'É') := tab.(Char.code 'E') + 2;
     tab.(Char.code 'Ö') := tab.(Char.code 'O') + 4;
     tab.(Char.code '?') := 3000;
  return fun x -> tab.(Char.code x)
;

value alphabetique n1 n2 =
  let rec boucle i1 i2 =
    if i1 == String.length n1 && i2 == String.length n2 then i1 - i2
    else if i1 == String.length n1 then -1
    else if i2 == String.length n2 then 1
    else
      let c1 = n1.[i1] in
      let c2 = n2.[i2] in
      if valeur_alphabetique c1 < valeur_alphabetique c2 then -1
      else if valeur_alphabetique c1 > valeur_alphabetique c2 then 1
      else boucle (succ i1) (succ i2)
  in
  if n1 = n2 then 0 else boucle (initiale n1) (initiale n2)
;

value map_title_strings f t =
  let t_name =
    match t.t_name with
    [ Tmain -> Tmain
    | Tname s -> Tname (f s)
    | Tnone -> Tnone ]
  in
  let t_ident = f t.t_ident in
  let t_place = f t.t_place in
  {t_name = t_name; t_ident = t_ident; t_place = t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end;
   t_nth = t.t_nth}
;

value map_relation_ps fp fs r =
  {r_type = r.r_type;
   r_fath = match r.r_fath with [ Some x -> Some (fp x) | None -> None ];
   r_moth = match r.r_moth with [ Some x -> Some (fp x) | None -> None ];
   r_sources = fs r.r_sources }
;

value map_person_ps fp fs p =
  {first_name = fs p.first_name; surname = fs p.surname; occ = p.occ;
   image = fs p.image; first_names_aliases = List.map fs p.first_names_aliases;
   surnames_aliases = List.map fs p.surnames_aliases;
   public_name = fs p.public_name;
   nick_names = List.map fs p.nick_names;
   titles = List.map (map_title_strings fs) p.titles;
   rparents = List.map (map_relation_ps fp fs) p.rparents;
   related = p.related;
   aliases = List.map fs p.aliases;
   occupation = fs p.occupation;
   sex = p.sex; access = p.access;
   birth = p.birth; birth_place = fs p.birth_place;
   birth_src = fs p.birth_src;
   baptism = p.baptism; baptism_place = fs p.baptism_place;
   baptism_src = fs p.baptism_src;
   death = p.death; death_place = fs p.death_place;
   death_src = fs p.death_src;
   burial = p.burial; burial_place = fs p.burial_place;
   burial_src = fs p.burial_src;
   family = p.family; notes = fs p.notes; psources = fs p.psources;
   cle_index = p.cle_index}
;

value map_family_ps fp fs fam =
  {marriage = fam.marriage; marriage_place = fs fam.marriage_place;
   marriage_src = fs fam.marriage_src; witnesses = Array.map fp fam.witnesses;
   not_married = fam.not_married; divorce = fam.divorce;
   children = Array.map fp fam.children;
   comment = fs fam.comment;
   origin_file = fs fam.origin_file;
   fsources = fs fam.fsources;
   fam_index = fam.fam_index}
;

value map_couple_p fp fam =
  {father = fp fam.father; mother = fp fam.mother}
;

(*
value string_of_place p =
  loop "" 0 where rec loop s i =
    if i == Array.length p then s
    else if s = "" then loop p.(i) (i + 1)
    else s ^ "," ^ loop p.(i) (i + 1)
;

value place_of_string s =
  loop [] 0 0 where rec loop list ibeg i =
    if i == String.length s then
      let list =
        if i == ibeg then list else [String.sub s ibeg (i - ibeg) :: list]
      in
      Array.of_list (List.rev list)
    else if s.[i] == ',' then
      let list = [String.sub s ibeg (i - ibeg) :: list] in
      loop list (i + 1) (i + 1)
    else loop list ibeg (i + 1)
;
*)
