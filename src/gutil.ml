(* $Id: gutil.ml,v 1.1 1998-09-01 14:32:04 ddr Exp $ *)

open Def;

value poi base i = base.persons.get (Adef.int_of_iper i);
value aoi base i = base.ascends.get (Adef.int_of_iper i);
value foi base i = base.families.get (Adef.int_of_ifam i);
value coi base i = base.couples.get (Adef.int_of_ifam i);
value sou base i = base.strings.get (Adef.int_of_istr i);

value bissextile a =
  if a mod 100 == 0 then a / 100 mod 4 == 0 else a mod 4 == 0
;

value nb_jours_dans_mois =
  let tb = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  fun m a -> if m == 2 && bissextile a then 29 else tb.(m - 1)
;

value temps_ecoule d1 d2 =
  match d1 with
  [ Djma j1 m1 a1 ->
      match d2 with
      [ Djma j2 m2 a2 ->
          let (jour, r) =
            if j1 <= j2 then (j2 - j1, 0)
            else (j2 - j1 + nb_jours_dans_mois m1 a1, 1)
          in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in Djma jour mois annee
      | Dma m2 a2 ->
          let r = 0 in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in Dma mois annee
      | Da sure a2 -> Da sure (a2 - a1) ]
  | Dma m1 a1 ->
      match d2 with
      [ Djma j2 m2 a2 ->
          let r = 0 in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in Dma mois annee
      | Dma m2 a2 ->
          let r = 0 in
          let (mois, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let annee = a2 - a1 - r in Dma mois annee
      | Da sure a2 -> Da sure (a2 - a1) ]
  | Da sure a1 ->
      match d2 with
      [ Djma _ _ a2 -> Da sure (a2 - a1)
      | Dma _ a2 -> Da sure (a2 - a1)
      | Da sure2 a2 ->
          Da (match sure with
              [ Sure -> sure2
              | About -> if sure2 = Sure then About else sure2
              | _ -> Maybe ])
             (a2 - a1) ] ]
;

value annee =
  fun
  [ Djma _ _ a -> a
  | Dma _ a -> a
  | Da _ a -> a ]
;

value strictement_avant d1 d2 =
  match temps_ecoule d2 d1 with
  [ Djma j m a -> a < 0
  | Dma m a -> a < 0
  | Da Sure a -> a < 0
  | Da _ a ->
      if a < 0 then True
      else if a > 0 then False
      else
        match (d1, d2) with
        [ (Da p1 _, Da p2 _) when p1 = p2 -> False
        | (Da Before _, _) | (_, Da After _) -> True
        | _ -> False ] ]
;

value strictement_apres d1 d2 =
  match temps_ecoule d1 d2 with
  [ Djma j m a -> a < 0
  | Dma m a -> a < 0
  | Da _ a ->
      if a < 0 then True
      else if a > 0 then False
      else
        match (d2, d1) with
        [ (Da p2 _, Da p1 _) when p1 = p2 -> False
        | (Da Before _, _) | (_, Da After _) -> True
        | _ -> False ] ]
;

value string_of_date =
  fun
  [ Djma j m a ->
      "on " ^ string_of_int j ^ "/" ^ string_of_int m ^ "/" ^ string_of_int a
  | Dma m a -> "in " ^ string_of_int m ^ "/" ^ string_of_int a
  | Da Sure a -> "in " ^ string_of_int a
  | Da About a -> "about " ^ string_of_int a
  | Da Maybe a -> "maybe in " ^ string_of_int a
  | Da Before a -> "before " ^ string_of_int a
  | Da After a -> "after " ^ string_of_int a
  | Da (OrYear a1) a -> "in " ^ string_of_int a ^ " or " ^ string_of_int a1 ]
;

value denomination base p =
  let prenom = sou base p.first_name in
  let nom = sou base p.surname in
  prenom ^
    (if p.occ == 0 || prenom = "?" || nom = "?" then ""
     else "." ^ string_of_int p.occ) ^
    " " ^ nom
;

value person_misc_names base p =
  if sou base p.first_name = "?" || sou base p.surname = "?" then [] else
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
  let first_names = [p.first_name :: p.first_names_aliases @ public_names] in
  let surnames = [p.surname :: p.surnames_aliases @ p.nick_names] in
  let surnames =
    if p.sexe == Feminin then
      List.fold_left
        (fun list ifam ->
           let cpl = coi base ifam in
           let husband = poi base cpl.father in
           if sou base husband.surname = "?" then
             husband.surnames_aliases @ list
           else [husband.surname :: husband.surnames_aliases @ list])
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
          let f = sou base f in
          List.fold_left (fun list s -> [f ^ " " ^ sou base s :: list]) list
            surnames)
    list first_names
  in
  let list =
    let first_names =
      List.map (sou base) [p.first_name :: p.first_names_aliases]
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
          List.map (sou base) [p.first_name :: p.first_names_aliases]
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
  let fn = Name.lower (sou base p.first_name ^ " " ^ sou base p.surname) in
  List.fold_left
    (fun list s ->
       let s = Name.lower s in
       if s = fn || List.mem s list then list else [s :: list])
    [] list
;

value person_ht_add base s ip = base.patch_name s ip;

value person_is_key base p k =
  let k = Name.crush_lower k in
  if k = Name.crush_lower (sou base p.first_name ^ " " ^ sou base p.surname)
  then True
  else if
    List.exists (fun x -> k = Name.crush_lower x) (person_misc_names base p)
  then True
  else False
;

value person_ht_find_unique base first_name surname occ =
  if first_name = "?" || surname = "?" then raise Not_found
  else
    let ipl = base.persons_of_name (first_name ^ " " ^ surname) in
    let first_name = Name.strip_lower first_name in
    let surname = Name.strip_lower surname in
    find ipl where rec find =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if occ == p.occ
          && first_name = Name.strip_lower (sou base p.first_name)
          && surname = Name.strip_lower (sou base p.surname)
          then p.cle_index
          else find ipl
      | _ -> raise Not_found ]
;

value lindex s c =
  pos 0 where rec pos i =
    if i == String.length s then None
    else if s.[i] == c then Some i else pos (i + 1)
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
      let ipl = base.persons_of_name s in
      select ipl where rec select =
        fun
        [ [ip :: ipl] ->
            if person_is_key base (poi base ip) s then [ip :: select ipl]
            else select ipl
        | [] -> [] ] ]
;

(* check base *)

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;
type base_error = error base_person;

type warning 'person =
  [ BirthAfterDeath of 'person
  | ChangedOrderOfChildren of base_family and array iper
  | ChildrenNotInOrder of base_family and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and Def.date
  | TitleDatesError of 'person and title istr
  | YoungForMarriage of 'person and Def.date ]
;
type base_warning = warning base_person;

type visit = [ NotVisited | BeingVisited | Visited ];

value check_noloop base error =
  let tab = Array.create (base.persons.len) NotVisited in
  let rec noloop i =
    match tab.(i) with
    [ NotVisited ->
        do match (base.ascends.get i).parents with
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
    | BeingVisited -> error (OwnAncestor (base.persons.get i))
    | Visited -> () ]
  in
  for i = 0 to base.persons.len - 1 do
    match tab.(i) with
    [ NotVisited -> noloop i
    | BeingVisited -> failwith "check_noloop algorithm error"
    | Visited -> () ];
  done
;

value check_noloop_for_person_list base error pl =
  let tab = Array.create (base.persons.len) NotVisited in
  let rec noloop p =
    let i = Adef.int_of_iper p.cle_index in
    match tab.(i) with
    [ NotVisited ->
        do match (aoi base p.cle_index).parents with
           [ Some ifam ->
               let cpl = coi base ifam in
               do tab.(i) := BeingVisited;
                  noloop (poi base cpl.father);
                  noloop (poi base cpl.mother);
               return ()
           | None -> () ];
           tab.(i) := Visited;
        return ()
    | BeingVisited -> error (OwnAncestor p)
    | Visited -> () ]
  in
  List.iter noloop pl
;

value child_born_after_his_parent base error warning x iparent =
  let parent = poi base iparent in
  match
    (Adef.od_of_codate parent.birth, Adef.od_of_codate x.birth, x.death)
  with
  [ (Some d1, Some d2, _) ->
      if strictement_apres d1 d2 then
        warning (ParentBornAfterChild parent x)
      else
        let a = temps_ecoule d1 d2 in
        if annee a < 12 then warning (ParentTooYoung parent a) else ()
  | (Some d1, _, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictement_apres d1 d2 then
        warning (ParentBornAfterChild parent x)
      else
        let a = temps_ecoule d1 d2 in
        if annee a < 12 then warning (ParentTooYoung parent a) else ()
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
  match (Adef.od_of_codate x.birth, father.death) with
  [ (Some d1, Death _ d2) ->
      match (d1, Adef.date_of_cdate d2) with
      [ (Da Before _, _) | (_, Da After _) -> ()
      | (d1, d2) ->
          if annee d1 > annee d2 + 1 then
            warning (DeadTooEarlyToBeFather father x)
          else () ]
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
             else if
               annee d2 > 1850 && annee (temps_ecoule d1 d2) < 13 then
               warning (YoungForMarriage p (temps_ecoule d1 d2))
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
  do match (poi base cpl.father).sexe with
     [ Masculin -> ()
     | _ -> error (BadSexOfMarriedPerson (poi base cpl.father)) ];
     match (poi base cpl.mother).sexe with
     [ Feminin -> ()
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
  do for i = 0 to base.persons.len - 1 do
       let p = base.persons.get i in
       check_person base error warning p;
     done;
     for i = 0 to base.families.len - 1 do
       let fam = base.families.get i in
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
  let t_title = f t.t_title in
  let t_place = f t.t_place in
  {t_name = t_name; t_title = t_title; t_place = t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end;
   t_nth = t.t_nth}
;

value map_person_strings f p =
  {first_name = f p.first_name; surname = f p.surname; occ = p.occ;
   photo = f p.photo; first_names_aliases = List.map f p.first_names_aliases;
   surnames_aliases = List.map f p.surnames_aliases;
   public_name = f p.public_name;
   nick_names = List.map f p.nick_names;
   titles = List.map (map_title_strings f) p.titles;
   aliases = List.map f p.aliases;
   occupation = f p.occupation;
   sexe = p.sexe; access = p.access;
   birth = p.birth; birth_place = f p.birth_place;
   baptism = p.baptism; baptism_place = f p.baptism_place;
   death = p.death; death_place = f p.death_place;
   burial = p.burial; burial_place = f p.burial_place;
   family = p.family; notes = f p.notes; psources = f p.psources;
   cle_index = p.cle_index}
;

value map_family_ps fp fs fam =
  {marriage = fam.marriage; marriage_place = fs fam.marriage_place;
   divorce = fam.divorce;
   children = Array.map fp fam.children;
   comment = fs fam.comment;
   origin_file = fs fam.origin_file;
   fsources = fs fam.fsources;
   fam_index = fam.fam_index}
;

value map_couple_p fp fam =
  {father = fp fam.father; mother = fp fam.mother}
;
