(* $Id: check.ml,v 5.28 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gwdb;
open Printf;

(* Printing check errors *)

value designation base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s =
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)
  in
  if first_name = "?" || surname = "?" then
    s ^ " (i=" ^ string_of_int (Adef.int_of_iper (get_key_index p)) ^ ")"
  else s
;

value print_base_error oc base =
  fun
  [ AlreadyDefined p ->
      fprintf oc "%s\nis defined several times\n" (designation base p)
  | OwnAncestor p ->
      fprintf oc "%s\nis his/her own ancestor\n" (designation base p)
  | BadSexOfMarriedPerson p ->
      fprintf oc "%s\n  bad sex for a married person\n" (designation base p) ]
;

value print_base_warning oc base =
  fun
  [ BigAgeBetweenSpouses fath moth a ->
      fprintf oc "The difference of age between %s and %s is quite important: %d\n" 
        (designation base fath) (designation base moth) a.year
  | BirthAfterDeath p ->
      fprintf oc "%s\n  born after his/her death\n" (designation base p)
  | ChangedOrderOfChildren ifam des _ _ ->
      let cpl = foi base ifam in
      fprintf oc "Changed order of children of %s and %s\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)))
  | ChildrenNotInOrder ifam des elder x -> do {
      let cpl = foi base ifam in
      fprintf oc
        "The following children of\n  %s\nand\n  %s\nare not in order:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf oc "- %s\n" (designation base elder);
      fprintf oc "- %s\n" (designation base x)
    }
  | ChangedOrderOfMarriages p _ _ ->
      fprintf oc "Changed order of marriages of %s\n" (designation base p)
  | CloseChildren ifam des elder x -> do {
      let cpl = foi base ifam in
      fprintf oc
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf oc "- %s\n" (designation base elder);
      fprintf oc "- %s\n" (designation base x)
    }
  | DeadOld p a ->
      fprintf oc "%s died at the advanced age of %d years old\n" 
        (designation base p) a.year
  | DeadTooEarlyToBeFather father child -> do {
      fprintf oc "%s\n" (designation base child);
      fprintf oc
        "  is born more than 2 years after the death of his/her father\n";
      fprintf oc "%s\n" (designation base father)
    }
  | IncoherentSex p fixed not_fixed -> do {
      fprintf oc "%s\n  sex not coherent with relations"
        (designation base p);
      if fixed > 0 then
        if not_fixed > 0 then
          fprintf oc " (fixed in %d of the %d cases)" fixed
            (fixed + not_fixed)
        else
          fprintf oc " (fixed)"
      else ();
      fprintf oc "\n";
    }
  | IncoherentAncestorDate anc p -> do {
      fprintf oc "%s\n" (designation base p);
      fprintf oc "  has a younger ancestor:\n";
      fprintf oc "%s\n" (designation base anc);
    }
  | MarriageDateAfterDeath p -> do {
      fprintf oc "%s\n" (designation base p);
      fprintf oc "marriage after his/her death\n"
    }
  | MarriageDateBeforeBirth p -> do {
      fprintf oc "%s\n" (designation base p);
      fprintf oc "marriage before his/her birth\n"
    }
  | MotherDeadAfterChildBirth mother child ->
      fprintf oc "%s\n  is born after the death of his/her mother\n%s\n"
        (designation base child) (designation base mother)
  | OldIndividual p a ->
      fprintf oc "%s is %d years old\n" (designation base p) a.year
  | ParentBornAfterChild parent child ->
      fprintf oc "%s born after his/her child %s\n"
        (designation base parent) (designation base child)
  | ParentTooOld p a ->
      fprintf oc "%s was parent at age of %d\n" (designation base p)
        a.year
  | ParentTooYoung p a ->
      fprintf oc "%s was parent at age of %d\n" (designation base p)
        a.year
  | TitleDatesError p t -> do {
      fprintf oc "%s\n" (designation base p);
      fprintf oc "has incorrect title dates as:\n";
      fprintf oc "  %s %s\n" (sou base t.t_ident) (sou base t.t_place)
    }
  | UndefinedSex _ ->
      ()
  | WitnessDateAfterDeath p -> do {
      fprintf oc "%s\n" (designation base p);
      fprintf oc "was witness after his/her death\n"
    }
  | WitnessDateBeforeBirth p -> do {
      fprintf oc "%s\n" (designation base p);
      fprintf oc "was witness before his/her birth\n"
    }
  | YoungForMarriage p a ->
      fprintf oc "%s married at age %d\n" (designation base p) a.year ]
;      

type stats =
  { men : mutable int;
    women : mutable int;
    neutre : mutable int;
    noname : mutable int;
    oldest_father : mutable (int * person);
    oldest_mother : mutable (int * person);
    youngest_father : mutable (int * person);
    youngest_mother : mutable (int * person);
    oldest_dead : mutable (int * person);
    oldest_still_alive : mutable (int * person) }
;

value birth_year p =
  match Adef.od_of_codate (get_birth p) with
  [ Some d ->
      match d with
      [ Dgreg {year = y; prec = Sure} _ -> Some y
      | _ -> None ]
  | _ -> None ]
;

value death_year current_year p =
  match get_death p with
  [ Death _ d ->
      match Adef.date_of_cdate d with
      [ Dgreg {year = y; prec = Sure} _ -> Some y
      | _ -> None ]
  | NotDead -> Some current_year
  | _ -> None ]
;

value update_stats base current_year s p = do {
  match get_sex p with
  [ Male -> s.men := s.men + 1
  | Female -> s.women := s.women + 1
  | Neuter -> s.neutre := s.neutre + 1 ];
  if p_first_name base p = "?" && p_surname base p = "?" then
    s.noname := s.noname + 1
  else ();
  match (birth_year p, death_year current_year p) with
  [ (Some y1, Some y2) -> do {
      let age = y2 - y1 in
      if age > fst s.oldest_dead && get_death p <> NotDead then
        s.oldest_dead := (age, p)
      else ();
      if age > fst s.oldest_still_alive && get_death p = NotDead then
        s.oldest_still_alive := (age, p)
      else ();
    }
  | _ -> () ];
  match (birth_year p, get_parents p) with
  [ (Some y2, Some ifam) -> do {
      let cpl = foi base ifam in
      match birth_year (poi base (get_father cpl)) with
      [ Some y1 -> do {
          let age = y2 - y1 in
          if age > fst s.oldest_father then
            s.oldest_father := (age, poi base (get_father cpl))
          else ();
          if age < fst s.youngest_father then
            s.youngest_father := (age, poi base (get_father cpl))
          else ();
        }
      | _ -> () ];
      match birth_year (poi base (get_mother cpl)) with
      [ Some y1 -> do {
          let age = y2 - y1 in
          if age > fst s.oldest_mother then
            s.oldest_mother := (age, poi base (get_mother cpl))
          else ();
          if age < fst s.youngest_mother then
            s.youngest_mother := (age, poi base (get_mother cpl))
          else ();
        }
      | _ -> () ];
    }
  | _ -> () ];
};

value min_year_of base p =
  match Adef.od_of_codate (get_birth p) with
  [ Some (Dgreg d _) -> Some d.year
  | Some (Dtext _) | None -> None ]
;

value rec check_ancestors base warning year year_tab ip ini_p =
  if fst year_tab.(Adef.int_of_iper ip) = max_int then do {
    let p = poi base ip in
    let new_year_o = min_year_of base p in
    let (new_year, new_ini_p, own_year) =
      match new_year_o with
      [ Some y -> (y, p, True)
      | None -> (year - 1, ini_p, False) ]
    in
    year_tab.(Adef.int_of_iper ip) := (new_year, own_year);
    if new_year >= year then warning (IncoherentAncestorDate p ini_p) else ();
    match get_parents p with
    [ Some ifam ->
        let fam = foi base ifam in
        List.iter
          (fun get ->
             let ip = get fam in
             let year = year_tab.(Adef.int_of_iper ip) in
             if fst year = max_int then
               check_ancestors base warning new_year year_tab ip new_ini_p
             else if snd year && fst year >= new_year then
               warning (IncoherentAncestorDate (poi base ip) new_ini_p)
             else ())
          [get_father; get_mother]
    | None -> () ];
  }
  else ()
;

value check_base_aux base error warning changed_p = do {
  eprintf "check persons\n";
  let nb_ind = nb_of_persons base in
  let year_tab = Array.create nb_ind (max_int, False) in
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do {
    ProgrBar.run i nb_ind;
    let p = poi base (Adef.iper_of_int i) in
    if fst year_tab.(i) = max_int then
      check_ancestors base warning max_int year_tab (Adef.iper_of_int i) p
    else ();
    match CheckItem.person base warning p with
    [ Some ippl -> List.iter changed_p ippl
    | None -> () ]
  };
  ProgrBar.finish ();
  eprintf "check families\n";
  let nb_fam = nb_of_families base in
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do {
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else CheckItem.family base error warning ifam fam
  };
  ProgrBar.finish ();
  Consang.check_noloop base error;
};

value check_base base error warning def changed_p pr_stats = do {
  let s =
    let y = (1000, poi base (Adef.iper_of_int 0)) in
    let o = (0, poi base (Adef.iper_of_int 0)) in
    {men = 0; women = 0; neutre = 0; noname = 0; oldest_father = o;
     oldest_mother = o; youngest_father = y; youngest_mother = y;
     oldest_dead = o; oldest_still_alive = o}
  in
  let current_year = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900 in
  check_base_aux base error warning changed_p;
  for i = 0 to nb_of_persons base - 1 do {
    let p = poi base (Adef.iper_of_int i) in
    if not (def i) then
      printf "Undefined: %s\n" (designation base p)
    else ();
    if pr_stats then update_stats base current_year s p else ();
    flush stdout;
  };
  if pr_stats then do {
    printf "\n";
    printf "%d men\n" s.men;
    printf "%d women\n" s.women;
    printf "%d unknown sex\n" s.neutre;
    printf "%d unnamed\n" s.noname;
    printf "Oldest: %s, %d\n" (designation base (snd s.oldest_dead))
      (fst s.oldest_dead);
    printf "Oldest still alive: %s, %d\n"
      (designation base (snd s.oldest_still_alive))
      (fst s.oldest_still_alive);
    printf "Youngest father: %s, %d\n"
      (designation base (snd s.youngest_father)) (fst s.youngest_father);
    printf "Youngest mother: %s, %d\n"
      (designation base (snd s.youngest_mother)) (fst s.youngest_mother);
    printf "Oldest father: %s, %d\n"
      (designation base (snd s.oldest_father)) (fst s.oldest_father);
    printf "Oldest mother: %s, %d\n"
      (designation base (snd s.oldest_mother)) (fst s.oldest_mother);
    printf "\n";
    flush stdout;
  }
  else ();
};
