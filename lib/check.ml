(* $Id: check.ml,v 5.28 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb
open Printf

(* Printing check errors *)

let designation base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s =
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)
  in
  if first_name = "?" || surname = "?" then
    s ^ " (i=" ^ string_of_int (Adef.int_of_iper (get_key_index p)) ^ ")"
  else s

let string_of_epers_name base epers_name =
  match epers_name with
    Epers_Birth -> "birth"
  | Epers_Baptism -> "baptism"
  | Epers_Death -> "death"
  | Epers_Burial -> "burial"
  | Epers_Cremation -> "cremation"
  | Epers_Accomplishment -> "accomplishment"
  | Epers_Acquisition -> "acquisition"
  | Epers_Adhesion -> "adhesion"
  | Epers_BaptismLDS -> "baptism (LDS)"
  | Epers_BarMitzvah -> "bar mitzvah"
  | Epers_BatMitzvah -> "bat mitzvah"
  | Epers_Benediction -> "benediction"
  | Epers_ChangeName -> "change name"
  | Epers_Circumcision -> "circumcision"
  | Epers_Confirmation -> "confirmation"
  | Epers_ConfirmationLDS -> "confirmation (LDS)"
  | Epers_Decoration -> "decoration"
  | Epers_DemobilisationMilitaire -> "military demobilisation"
  | Epers_Diploma -> "diploma"
  | Epers_Distinction -> "distinction"
  | Epers_Dotation -> "dotation"
  | Epers_DotationLDS -> "dotation (LDS)"
  | Epers_Education -> "education"
  | Epers_Election -> "election"
  | Epers_Emigration -> "emigration"
  | Epers_Excommunication -> "excommunication"
  | Epers_FamilyLinkLDS -> "family link (LDS)"
  | Epers_FirstCommunion -> "first communion"
  | Epers_Funeral -> "funeral"
  | Epers_Graduate -> "graduation"
  | Epers_Hospitalisation -> "hospitalisation"
  | Epers_Illness -> "illness"
  | Epers_Immigration -> "immigration"
  | Epers_ListePassenger -> "passenger liste"
  | Epers_MilitaryDistinction -> "military distinction"
  | Epers_MilitaryPromotion -> "military promotion"
  | Epers_MilitaryService -> "military service"
  | Epers_MobilisationMilitaire -> "military mobilisation"
  | Epers_Naturalisation -> "naturalisation"
  | Epers_Occupation -> "occupation"
  | Epers_Ordination -> "ordination"
  | Epers_Property -> "property"
  | Epers_Recensement -> "recensement"
  | Epers_Residence -> "residence"
  | Epers_Retired -> "retirement"
  | Epers_ScellentChildLDS -> "scellent child (LDS)"
  | Epers_ScellentParentLDS -> "scellent parent (LDS)"
  | Epers_ScellentSpouseLDS -> "scellent spouse (LDS)"
  | Epers_VenteBien -> "sell"
  | Epers_Will -> "will"
  | Epers_Name n -> sou base n

let string_of_efam_name base efam_name =
  match efam_name with
    Efam_Marriage -> "marriage"
  | Efam_NoMarriage -> "relation"
  | Efam_NoMention -> "relation"
  | Efam_Engage -> "engagement"
  | Efam_Divorce -> "divorce"
  | Efam_Separated -> "separation"
  | Efam_Annulation -> "annulation"
  | Efam_MarriageBann -> "marriage bann"
  | Efam_MarriageContract -> "marriage contract"
  | Efam_MarriageLicense -> "marriage licence"
  | Efam_PACS -> "PACS"
  | Efam_Residence -> "residence"
  | Efam_Name n -> sou base n

let print_base_error oc base =
  function
    AlreadyDefined p ->
      fprintf oc "%s\nis defined several times\n" (designation base p)
  | OwnAncestor p ->
      fprintf oc "%s\nis his/her own ancestor\n" (designation base p)
  | BadSexOfMarriedPerson p ->
      fprintf oc "%s\n  bad sex for a married person\n" (designation base p)

let print_base_warning oc base =
  function
    BigAgeBetweenSpouses (fath, moth, a) ->
      fprintf oc
        "The difference of age between %s and %s is quite important: %d\n"
        (designation base fath) (designation base moth) a.year
  | BirthAfterDeath p ->
      fprintf oc "%s\n  born after his/her death\n" (designation base p)
  | ChangedOrderOfChildren (ifam, des, _, _) ->
      let cpl = foi base ifam in
      fprintf oc "Changed order of children of %s and %s\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)))
  | ChildrenNotInOrder (ifam, des, elder, x) ->
      let cpl = foi base ifam in
      fprintf oc
        "The following children of\n  %s\nand\n  %s\nare not in order:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf oc "- %s\n" (designation base elder);
      fprintf oc "- %s\n" (designation base x)
  | ChangedOrderOfMarriages (p, _, _) ->
      fprintf oc "Changed order of marriages of %s\n" (designation base p)
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      let cpl = foi base ifam in
      fprintf oc "Changed order of family's events for %s\n"
        (designation base (poi base (get_father cpl)));
      fprintf oc "Changed order of family's events for %s\n"
        (designation base (poi base (get_mother cpl)))
  | ChangedOrderOfPersonEvents (p, _, _) ->
      fprintf oc "Changed order of person's events for %s\n"
        (designation base p)
  | CloseChildren (ifam, des, elder, x) ->
      let cpl = foi base ifam in
      fprintf oc
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf oc "- %s\n" (designation base elder);
      fprintf oc "- %s\n" (designation base x)
  | DeadOld (p, a) ->
      fprintf oc "%s died at the advanced age of %d years old\n"
        (designation base p) a.year
  | DeadTooEarlyToBeFather (father, child) ->
      fprintf oc "%s\n" (designation base child);
      fprintf oc
        "  is born more than 2 years after the death of his/her father\n";
      fprintf oc "%s\n" (designation base father)
  | FEventOrder (p, e1, e2) ->
      fprintf oc "%s's %s before his/her %s\n" (designation base p)
        (string_of_efam_name base e1.efam_name)
        (string_of_efam_name base e2.efam_name)
  | FWitnessEventAfterDeath (p, e) ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "witnessed the %s after his/her death\n"
        (string_of_efam_name base e.efam_name)
  | FWitnessEventBeforeBirth (p, e) ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "witnessed the %s before his/her birth\n"
        (string_of_efam_name base e.efam_name)
  | IncoherentSex (p, fixed, not_fixed) ->
      fprintf oc "%s\n  sex not coherent with relations" (designation base p);
      if fixed > 0 then
        if not_fixed > 0 then
          fprintf oc " (fixed in %d of the %d cases)" fixed
            (fixed + not_fixed)
        else fprintf oc " (fixed)";
      fprintf oc "\n"
  | IncoherentAncestorDate (anc, p) ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "  has a younger ancestor:\n";
      fprintf oc "%s\n" (designation base anc)
  | MarriageDateAfterDeath p ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "marriage after his/her death\n"
  | MarriageDateBeforeBirth p ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "marriage before his/her birth\n"
  | MotherDeadAfterChildBirth (mother, child) ->
      fprintf oc "%s\n  is born after the death of his/her mother\n%s\n"
        (designation base child) (designation base mother)
  | ParentBornAfterChild (parent, child) ->
      fprintf oc "%s born after his/her child %s\n" (designation base parent)
        (designation base child)
  | ParentTooOld (p, a) ->
      fprintf oc "%s was parent at age of %d\n" (designation base p) a.year
  | ParentTooYoung (p, a) ->
      fprintf oc "%s was parent at age of %d\n" (designation base p) a.year
  | PEventOrder (p, e1, e2) ->
      fprintf oc "%s's %s before his/her %s\n" (designation base p)
        (string_of_epers_name base e1.epers_name)
        (string_of_epers_name base e2.epers_name)
  | PWitnessEventAfterDeath (p, e) ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "witnessed the %s after his/her death\n"
        (string_of_epers_name base e.epers_name)
  | PWitnessEventBeforeBirth (p, e) ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "witnessed the %s before his/her birth\n"
        (string_of_epers_name base e.epers_name)
  | TitleDatesError (p, t) ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "has incorrect title dates as:\n";
      fprintf oc "  %s %s\n" (sou base t.t_ident) (sou base t.t_place)
  | UndefinedSex _ -> ()
  | WitnessDateAfterDeath p ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "was witness after his/her death\n"
  | WitnessDateBeforeBirth p ->
      fprintf oc "%s\n" (designation base p);
      fprintf oc "was witness before his/her birth\n"
  | YoungForMarriage (p, a) ->
      fprintf oc "%s married at age %d\n" (designation base p) a.year

type stats =
  { mutable men : int;
    mutable women : int;
    mutable neutre : int;
    mutable noname : int;
    mutable oldest_father : int * person;
    mutable oldest_mother : int * person;
    mutable youngest_father : int * person;
    mutable youngest_mother : int * person;
    mutable oldest_dead : int * person;
    mutable oldest_still_alive : int * person }

let birth_year p =
  match Adef.od_of_codate (get_birth p) with
    Some d ->
      begin match d with
        Dgreg ({year = y; prec = Sure}, _) -> Some y
      | _ -> None
      end
  | _ -> None

let death_year current_year p =
  match get_death p with
    Death (_, d) ->
      begin match Adef.date_of_cdate d with
        Dgreg ({year = y; prec = Sure}, _) -> Some y
      | _ -> None
      end
  | NotDead -> Some current_year
  | _ -> None

let update_stats base current_year s p =
  begin match get_sex p with
    Male -> s.men <- s.men + 1
  | Female -> s.women <- s.women + 1
  | Neuter -> s.neutre <- s.neutre + 1
  end;
  if p_first_name base p = "?" && p_surname base p = "?" then
    s.noname <- s.noname + 1;
  begin match birth_year p, death_year current_year p with
    Some y1, Some y2 ->
      let age = y2 - y1 in
      if age > fst s.oldest_dead && get_death p <> NotDead then
        s.oldest_dead <- age, p;
      if age > fst s.oldest_still_alive && get_death p = NotDead then
        s.oldest_still_alive <- age, p
  | _ -> ()
  end;
  match birth_year p, get_parents p with
    Some y2, Some ifam ->
      let cpl = foi base ifam in
      begin match birth_year (poi base (get_father cpl)) with
        Some y1 ->
          let age = y2 - y1 in
          if age > fst s.oldest_father then
            s.oldest_father <- age, poi base (get_father cpl);
          if age < fst s.youngest_father then
            s.youngest_father <- age, poi base (get_father cpl)
      | _ -> ()
      end;
      begin match birth_year (poi base (get_mother cpl)) with
        Some y1 ->
          let age = y2 - y1 in
          if age > fst s.oldest_mother then
            s.oldest_mother <- age, poi base (get_mother cpl);
          if age < fst s.youngest_mother then
            s.youngest_mother <- age, poi base (get_mother cpl)
      | _ -> ()
      end
  | _ -> ()

let min_year_of base p =
  match Adef.od_of_codate (get_birth p) with
    Some (Dgreg (d, _)) -> Some d.year
  | Some (Dtext _) | None -> None

let rec check_ancestors base warning year year_tab ip ini_p =
  if fst year_tab.(Adef.int_of_iper ip) = max_int then
    let p = poi base ip in
    let new_year_o = min_year_of base p in
    let (new_year, new_ini_p, own_year) =
      match new_year_o with
        Some y -> y, p, true
      | None -> year - 1, ini_p, false
    in
    year_tab.(Adef.int_of_iper ip) <- new_year, own_year;
    if new_year >= year then warning (IncoherentAncestorDate (p, ini_p));
    match get_parents p with
      Some ifam ->
        let fam = foi base ifam in
        List.iter
          (fun get ->
             let ip = get fam in
             let year = year_tab.(Adef.int_of_iper ip) in
             if fst year = max_int then
               check_ancestors base warning new_year year_tab ip new_ini_p
             else if snd year && fst year >= new_year then
               warning (IncoherentAncestorDate (poi base ip, new_ini_p)))
          [get_father; get_mother]
    | None -> ()

let check_base_aux base error warning changed_p =
  eprintf "check persons\n";
  let nb_ind = nb_of_persons base in
  let year_tab = Array.make nb_ind (max_int, false) in
  ProgrBar.start ();
  for i = 0 to nb_ind - 1 do
    ProgrBar.run i nb_ind;
    let p = poi base (Adef.iper_of_int i) in
    if fst year_tab.(i) = max_int then
      check_ancestors base warning max_int year_tab (Adef.iper_of_int i) p;
    match CheckItem.person base warning p with
      Some ippl -> List.iter changed_p ippl
    | None -> ()
  done;
  ProgrBar.finish ();
  eprintf "check families\n";
  let nb_fam = nb_of_families base in
  ProgrBar.start ();
  for i = 0 to nb_fam - 1 do
    ProgrBar.run i nb_fam;
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if is_deleted_family fam then ()
    else CheckItem.family base error warning ifam fam
  done;
  ProgrBar.finish ();
  Consang.check_noloop base error

let check_base base error warning def changed_p pr_stats =
  let s =
    let y = 1000, poi base (Adef.iper_of_int 0) in
    let o = 0, poi base (Adef.iper_of_int 0) in
    {men = 0; women = 0; neutre = 0; noname = 0; oldest_father = o;
     oldest_mother = o; youngest_father = y; youngest_mother = y;
     oldest_dead = o; oldest_still_alive = o}
  in
  let current_year = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900 in
  check_base_aux base error warning changed_p;
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if not (def i) then printf "Undefined: %s\n" (designation base p);
    if pr_stats then update_stats base current_year s p;
    flush stdout
  done;
  if pr_stats then
    begin
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
      flush stdout
    end
