(* $Id: check.ml,v 5.28 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* Printing check errors *)

let designation base p =
  let s = Gutil.designation base p in
  if String.get s 0 = '?' || String.get s (String.length s - 1) = '?' then
    s ^ " (i=" ^ Gwdb.string_of_iper (Gwdb.get_iper p) ^ ")"
  else s

let string_of_epers_name base epers_name =
  match epers_name with
  | Def.Epers_Birth -> "birth"
  | Def.Epers_Baptism -> "baptism"
  | Def.Epers_Death -> "death"
  | Def.Epers_Burial -> "burial"
  | Def.Epers_Cremation -> "cremation"
  | Def.Epers_Accomplishment -> "accomplishment"
  | Def.Epers_Acquisition -> "acquisition"
  | Def.Epers_Adhesion -> "adhesion"
  | Def.Epers_BaptismLDS -> "baptism (LDS)"
  | Def.Epers_BarMitzvah -> "bar mitzvah"
  | Def.Epers_BatMitzvah -> "bat mitzvah"
  | Def.Epers_Benediction -> "benediction"
  | Def.Epers_ChangeName -> "change name"
  | Def.Epers_Circumcision -> "circumcision"
  | Def.Epers_Confirmation -> "confirmation"
  | Def.Epers_ConfirmationLDS -> "confirmation (LDS)"
  | Def.Epers_Decoration -> "decoration"
  | Def.Epers_DemobilisationMilitaire -> "military demobilisation"
  | Def.Epers_Diploma -> "diploma"
  | Def.Epers_Distinction -> "distinction"
  | Def.Epers_Dotation -> "dotation"
  | Def.Epers_DotationLDS -> "dotation (LDS)"
  | Def.Epers_Education -> "education"
  | Def.Epers_Election -> "election"
  | Def.Epers_Emigration -> "emigration"
  | Def.Epers_Excommunication -> "excommunication"
  | Def.Epers_FamilyLinkLDS -> "family link (LDS)"
  | Def.Epers_FirstCommunion -> "first communion"
  | Def.Epers_Funeral -> "funeral"
  | Def.Epers_Graduate -> "graduation"
  | Def.Epers_Hospitalisation -> "hospitalisation"
  | Def.Epers_Illness -> "illness"
  | Def.Epers_Immigration -> "immigration"
  | Def.Epers_ListePassenger -> "passenger liste"
  | Def.Epers_MilitaryDistinction -> "military distinction"
  | Def.Epers_MilitaryPromotion -> "military promotion"
  | Def.Epers_MilitaryService -> "military service"
  | Def.Epers_MobilisationMilitaire -> "military mobilisation"
  | Def.Epers_Naturalisation -> "naturalisation"
  | Def.Epers_Occupation -> "occupation"
  | Def.Epers_Ordination -> "ordination"
  | Def.Epers_Property -> "property"
  | Def.Epers_Recensement -> "recensement"
  | Def.Epers_Residence -> "residence"
  | Def.Epers_Retired -> "retirement"
  | Def.Epers_ScellentChildLDS -> "scellent child (LDS)"
  | Def.Epers_ScellentParentLDS -> "scellent parent (LDS)"
  | Def.Epers_ScellentSpouseLDS -> "scellent spouse (LDS)"
  | Def.Epers_VenteBien -> "sell"
  | Def.Epers_Will -> "will"
  | Def.Epers_Name n -> Gwdb.sou base n
  | Def.Epers_Adoption -> "adoption"

let string_of_efam_name base efam_name =
  match efam_name with
  | Def.Efam_Marriage -> "marriage"
  | Def.Efam_NoMarriage -> "relation"
  | Def.Efam_NoMention -> "relation"
  | Def.Efam_Engage -> "engagement"
  | Def.Efam_Divorce -> "divorce"
  | Def.Efam_Separated -> "separation"
  | Def.Efam_Annulation -> "annulation"
  | Def.Efam_MarriageBann -> "marriage bann"
  | Def.Efam_MarriageContract -> "marriage contract"
  | Def.Efam_MarriageLicense -> "marriage licence"
  | Def.Efam_PACS -> "PACS"
  | Def.Efam_Residence -> "residence"
  | Def.Efam_Name n -> Gwdb.sou base n

let print_base_error oc base = function
  | Def.AlreadyDefined p ->
      Printf.fprintf oc "%s is defined several times\n" (designation base p)
  | Def.OwnAncestor p ->
      Printf.fprintf oc "%s is his/her own ancestor\n" (designation base p)
  | Def.BadSexOfMarriedPerson p ->
      Printf.fprintf oc "%s bad sex for a married person\n" (designation base p)

let print_base_warning oc base (w : Warning.base_warning) =
  let get_fevent_name e = e.Def.efam_name in
  let get_pevent_name e = e.Def.epers_name in
  match w with
  | Warning.BigAgeBetweenSpouses (p1, p2, a) ->
      Printf.fprintf oc
        "The difference of age between %s and %s is quite important: %d\n"
        (designation base p1) (designation base p2) a.year
  | BirthAfterDeath p ->
      Printf.fprintf oc "%s born after his/her death\n" (designation base p)
  | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = Gwdb.foi base ifam in
      Printf.fprintf oc "Changed order of children of %s and %s\n"
        (designation base (Gwdb.poi base (Gwdb.get_father cpl)))
        (designation base (Gwdb.poi base (Gwdb.get_mother cpl)))
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      let cpl = Gwdb.foi base ifam in
      Printf.fprintf oc
        "The following children of\n  %s\nand\n  %s\nare not in order:\n"
        (designation base (Gwdb.poi base (Gwdb.get_father cpl)))
        (designation base (Gwdb.poi base (Gwdb.get_mother cpl)));
      Printf.fprintf oc "- %s\n" (designation base elder);
      Printf.fprintf oc "- %s\n" (designation base x)
  | ChangedOrderOfMarriages (p, _, _) ->
      Printf.fprintf oc "Changed order of marriages of %s\n"
        (designation base p)
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      let cpl = Gwdb.foi base ifam in
      Printf.fprintf oc "Changed order of family's events for %s\n"
        (designation base (Gwdb.poi base (Gwdb.get_father cpl)));
      Printf.fprintf oc "Changed order of family's events for %s\n"
        (designation base (Gwdb.poi base (Gwdb.get_mother cpl)))
  | ChangedOrderOfPersonEvents (p, _, _) ->
      Printf.fprintf oc "Changed order of person's events for %s\n"
        (designation base p)
  | CloseChildren (ifam, c1, c2) ->
      let cpl = Gwdb.foi base ifam in
      Printf.fprintf oc
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (Gwdb.poi base (Gwdb.get_father cpl)))
        (designation base (Gwdb.poi base (Gwdb.get_mother cpl)));
      Printf.fprintf oc "- %s\n" (designation base c1);
      Printf.fprintf oc "- %s\n" (designation base c2)
  | DeadOld (p, a) ->
      Printf.fprintf oc "%s died at the advanced age of %d years old\n"
        (designation base p) a.year
  | DeadTooEarlyToBeFather (father, child) ->
      Printf.fprintf oc "%s " (designation base child);
      Printf.fprintf oc
        "is born more than 2 years after the death of his/her father";
      Printf.fprintf oc " %s\n" (designation base father)
  | DistantChildren (ifam, p1, p2) ->
      let cpl = Gwdb.foi base ifam in
      Printf.fprintf oc
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (Gwdb.poi base (Gwdb.get_father cpl)))
        (designation base (Gwdb.poi base (Gwdb.get_mother cpl)));
      Printf.fprintf oc "- %s\n" (designation base p1);
      Printf.fprintf oc "- %s\n" (designation base p2)
  | FEventOrder (p, e1, e2) ->
      Printf.fprintf oc "%s's %s before his/her %s\n" (designation base p)
        (string_of_efam_name base (get_fevent_name e1))
        (string_of_efam_name base (get_fevent_name e2))
  | FWitnessEventAfterDeath (p, e, _fam) ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "witnessed the %s after his/her death\n"
        (string_of_efam_name base (get_fevent_name e))
  | FWitnessEventBeforeBirth (p, e, _fam) ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "witnessed the %s before his/her birth\n"
        (string_of_efam_name base (get_fevent_name e))
  | IncoherentSex (p, fixed, not_fixed) ->
      Printf.fprintf oc "%s sex not coherent with relations"
        (designation base p);
      if fixed > 0 then
        if not_fixed > 0 then
          Printf.fprintf oc " (fixed in %d of the %d cases)" fixed
            (fixed + not_fixed)
        else Printf.fprintf oc " (fixed)";
      Printf.fprintf oc "\n"
  | IncoherentAncestorDate (anc, p) ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "  has a younger ancestor:";
      Printf.fprintf oc " %s\n" (designation base anc)
  | MarriageDateAfterDeath p ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "marriage after his/her death\n"
  | MarriageDateBeforeBirth p ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "marriage before his/her birth\n"
  | MotherDeadBeforeChildBirth (mother, child) ->
      Printf.fprintf oc "%s is born after the death of his/her mother %s\n"
        (designation base child) (designation base mother)
  | ParentBornAfterChild (parent, child) ->
      Printf.fprintf oc "%s born after his/her child %s\n"
        (designation base parent) (designation base child)
  | ParentTooOld (p, a, _) ->
      Printf.fprintf oc "%s was parent at age of %d\n" (designation base p)
        a.year
  | ParentTooYoung (p, a, _) ->
      Printf.fprintf oc "%s was parent at age of %d\n" (designation base p)
        a.year
  | PossibleDuplicateFam (f1, f2) ->
      Printf.fprintf oc "possible duplicate families: %s and %s\n"
        (Gwdb.string_of_ifam f1) (Gwdb.string_of_ifam f2)
  | PossibleDuplicateFamHomonymous (f1, f2, p) ->
      let f = Gwdb.foi base f1 in
      let fath = Gwdb.get_father f in
      let moth = Gwdb.get_mother f in
      let curr, hom =
        if Gwdb.eq_iper fath (Gwdb.get_iper p) then (moth, fath)
        else (fath, moth)
      in
      Printf.fprintf oc
        "possible duplicate families: %s and %s, %s has unions with several \
         persons named %s\n"
        (Gwdb.string_of_ifam f1) (Gwdb.string_of_ifam f2)
        (designation base (Gwdb.poi base curr))
        (designation base (Gwdb.poi base hom))
  | PEventOrder (p, e1, e2) ->
      Printf.fprintf oc "%s's %s before his/her %s\n" (designation base p)
        (string_of_epers_name base (get_pevent_name e1))
        (string_of_epers_name base (get_pevent_name e2))
  | PWitnessEventAfterDeath (p, e, _origin) ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "witnessed the %s after his/her death\n"
        (string_of_epers_name base (get_pevent_name e))
  | PWitnessEventBeforeBirth (p, e, _origin) ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "witnessed the %s before his/her birth\n"
        (string_of_epers_name base (get_pevent_name e))
  | TitleDatesError (p, t) ->
      Printf.fprintf oc "%s " (designation base p);
      Printf.fprintf oc "has incorrect title dates as:\n";
      Printf.fprintf oc " %s %s\n"
        (Gwdb.sou base t.Def.t_ident)
        (Gwdb.sou base t.Def.t_place)
  | UndefinedSex p ->
      Printf.fprintf oc "Undefined sex for %s\n" (designation base p)
  | YoungForMarriage (p, a, _) | OldForMarriage (p, a, _) ->
      Printf.fprintf oc "%s married at age %d\n" (designation base p) a.year

type check_date =
  | CheckBefore of int
  | CheckAfter of int
  | CheckOther of int
  | CheckInfered of check_date

let min_year_of p =
  let aux = function
    | { Date.prec = After; year } -> CheckAfter year
    | { prec = Before; year } -> CheckBefore year
    | { year } -> CheckOther year
  in
  Option.map aux (Date.cdate_to_dmy_opt (Gwdb.get_birth p))

let dummy_date = CheckInfered (CheckOther max_int)

(* check ad print warning if ancestors is born before person *)
let rec check_ancestors base warning year year_tab ip ini_p =
  let infer = function
    | CheckBefore i -> CheckInfered (CheckBefore (pred i))
    | CheckAfter i -> CheckInfered (CheckAfter (pred i))
    | CheckOther i -> CheckInfered (CheckOther (pred i))
    | CheckInfered (CheckBefore i) -> CheckInfered (CheckBefore (pred i))
    | CheckInfered (CheckAfter i) -> CheckInfered (CheckAfter (pred i))
    | CheckInfered (CheckOther i) -> CheckInfered (CheckOther (pred i))
    | _ -> assert false
  in
  let own = function CheckInfered _ -> false | _ -> true in
  let test a b p p' =
    match (a, b) with
    | ( CheckAfter y,
        ( CheckBefore y'
        | CheckOther y'
        | CheckInfered (CheckBefore y')
        | CheckInfered (CheckOther y') ) )
      when y >= y' ->
        warning (Warning.IncoherentAncestorDate (Lazy.force p, p'))
    | _ -> ()
  in
  if Gwdb.Marker.get year_tab ip = dummy_date then (
    let p = Gwdb.poi base ip in
    let new_year, new_ini_p =
      match min_year_of p with Some y -> (y, p) | None -> (infer year, ini_p)
    in
    Gwdb.Marker.set year_tab ip new_year;
    test new_year year (lazy p) ini_p;
    match Gwdb.get_parents p with
    | Some ifam ->
        let fam = Gwdb.foi base ifam in
        let f ip =
          let year = Gwdb.Marker.get year_tab ip in
          if year = dummy_date then
            check_ancestors base warning new_year year_tab ip new_ini_p
          else if own year then
            test year new_year (lazy (Gwdb.poi base ip)) new_ini_p
        in
        f @@ Gwdb.get_father fam;
        f @@ Gwdb.get_mother fam
    | None -> ())

let check_base ?(verbose = false) ?(mem = false) base error warning changed_p =
  if not mem then (
    Gwdb.load_persons_array base;
    Gwdb.load_ascends_array base;
    Gwdb.load_unions_array base;
    Gwdb.load_couples_array base);
  let persons = Gwdb.ipers base in
  let len = Gwdb.Collection.length persons in
  let year_tab = Gwdb.iper_marker (Gwdb.ipers base) dummy_date in
  if verbose then (
    Printf.eprintf "check persons\n";
    ProgrBar.start ();
    Gwdb.Collection.iteri
      (fun i ip ->
        ProgrBar.run i len;
        let p = Gwdb.poi base ip in
        if Gwdb.Marker.get year_tab ip = dummy_date then
          check_ancestors base warning dummy_date year_tab ip p;
        match CheckItem.person ~onchange:false base warning p with
        | Some ippl -> List.iter changed_p ippl
        | None -> ())
      persons;
    ProgrBar.finish ())
  else
    Gwdb.Collection.iter
      (fun ip ->
        let p = Gwdb.poi base ip in
        if Gwdb.Marker.get year_tab ip = dummy_date then
          check_ancestors base warning dummy_date year_tab ip p;
        match CheckItem.person ~onchange:false base warning p with
        | Some ippl -> List.iter changed_p ippl
        | None -> ())
      persons;
  if not mem then (
    Gwdb.clear_unions_array base;
    Gwdb.load_families_array base;
    Gwdb.load_descends_array base);
  let families = Gwdb.ifams base in
  let len = Gwdb.Collection.length families in
  if verbose then (
    Printf.eprintf "check families\n";
    ProgrBar.start ();
    Gwdb.Collection.iteri
      (fun i ifam ->
        ProgrBar.run i len;
        CheckItem.family ~onchange:false base warning ifam @@ Gwdb.foi base ifam)
      families;
    ProgrBar.finish ())
  else
    Gwdb.Collection.iter
      (fun ifam ->
        CheckItem.family ~onchange:false base warning ifam @@ Gwdb.foi base ifam)
      families;
  if not mem then (
    Gwdb.clear_persons_array base;
    Gwdb.clear_families_array base;
    Gwdb.clear_descends_array base);
  Consang.check_noloop base error;
  if not mem then (
    Gwdb.clear_ascends_array base;
    Gwdb.clear_couples_array base)
