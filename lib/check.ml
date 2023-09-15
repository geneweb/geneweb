(* $Id: check.ml,v 5.28 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

(* Printing check errors *)

let designation base p =
  let s = Gutil.designation base p in
  if String.get s 0 = '?' || String.get s (String.length s - 1) = '?' then
    s ^ " (i=" ^ string_of_iper (get_iper p) ^ ")"
  else s

let string_of_epers_name base epers_name =
  match epers_name with
  | Epers_Birth -> "birth"
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
  | Efam_Marriage -> "marriage"
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

let print_base_error oc base = function
  | AlreadyDefined p ->
      Printf.fprintf oc "%s is defined several times\n" (designation base p)
  | OwnAncestor p ->
      Printf.fprintf oc "%s is his/her own ancestor\n" (designation base p)
  | BadSexOfMarriedPerson p ->
      Printf.fprintf oc "%s bad sex for a married person\n" (designation base p)

let print_base_warning fmt base (w : CheckItem.base_warning) =
  let get_fevent_name e = e.efam_name in
  let get_pevent_name e = e.epers_name in
  let fprintf = Format.fprintf in
  match w with
  | Warning.BigAgeBetweenSpouses (p1, p2, a) ->
      fprintf fmt
        "The difference of age between %s and %s is quite important: %d\n"
        (designation base p1) (designation base p2) a.year
  | BirthAfterDeath p ->
      fprintf fmt "%s born after his/her death\n" (designation base p)
  | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = foi base ifam in
      fprintf fmt "Changed order of children of %s and %s\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)))
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      fprintf fmt
        "The following children of\n  %s\nand\n  %s\nare not in order:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf fmt "- %s\n" (designation base elder);
      fprintf fmt "- %s\n" (designation base x)
  | ChangedOrderOfMarriages (p, _, _) ->
      fprintf fmt "Changed order of marriages of %s\n" (designation base p)
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      let cpl = foi base ifam in
      fprintf fmt "Changed order of family's events for %s\n"
        (designation base (poi base (get_father cpl)));
      fprintf fmt "Changed order of family's events for %s\n"
        (designation base (poi base (get_mother cpl)))
  | ChangedOrderOfPersonEvents (p, _, _) ->
      fprintf fmt "Changed order of person's events for %s\n"
        (designation base p)
  | CloseChildren (ifam, c1, c2) ->
      let cpl = foi base ifam in
      fprintf fmt
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf fmt "- %s\n" (designation base c1);
      fprintf fmt "- %s\n" (designation base c2)
  | DeadOld (p, a) ->
      fprintf fmt "%s died at the advanced age of %d years old\n"
        (designation base p) a.year
  | DeadTooEarlyToBeFather (father, child) ->
      fprintf fmt "%s " (designation base child);
      fprintf fmt "is born more than 2 years after the death of his/her father";
      fprintf fmt " %s\n" (designation base father)
  | DistantChildren (ifam, p1, p2) ->
      let cpl = foi base ifam in
      fprintf fmt
        "The following children of\n  %s\nand\n  %s\nare born very close:\n"
        (designation base (poi base (get_father cpl)))
        (designation base (poi base (get_mother cpl)));
      fprintf fmt "- %s\n" (designation base p1);
      fprintf fmt "- %s\n" (designation base p2)
  | FEventOrder (p, e1, e2) ->
      fprintf fmt "%s's %s before his/her %s\n" (designation base p)
        (string_of_efam_name base (get_fevent_name e1))
        (string_of_efam_name base (get_fevent_name e2))
  | FWitnessEventAfterDeath (p, e, _fam) ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "witnessed the %s after his/her death\n"
        (string_of_efam_name base (get_fevent_name e))
  | FWitnessEventBeforeBirth (p, e, _fam) ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "witnessed the %s before his/her birth\n"
        (string_of_efam_name base (get_fevent_name e))
  | IncoherentSex (p, fixed, not_fixed) ->
      fprintf fmt "%s sex not coherent with relations" (designation base p);
      if fixed > 0 then
        if not_fixed > 0 then
          fprintf fmt " (fixed in %d of the %d cases)" fixed (fixed + not_fixed)
        else fprintf fmt " (fixed)";
      fprintf fmt "\n"
  | IncoherentAncestorDate (anc, p) ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "  has a younger ancestor:";
      fprintf fmt " %s\n" (designation base anc)
  | MarriageDateAfterDeath p ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "marriage after his/her death\n"
  | MarriageDateBeforeBirth p ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "marriage before his/her birth\n"
  | MotherDeadBeforeChildBirth (mother, child) ->
      fprintf fmt "%s is born after the death of his/her mother %s\n"
        (designation base child) (designation base mother)
  | ParentBornAfterChild (parent, child) ->
      fprintf fmt "%s born after his/her child %s\n" (designation base parent)
        (designation base child)
  | ParentTooOld (p, a, _) ->
      fprintf fmt "%s was parent at age of %d\n" (designation base p) a.year
  | ParentTooYoung (p, a, _) ->
      fprintf fmt "%s was parent at age of %d\n" (designation base p) a.year
  | PossibleDuplicateFam (f1, f2) ->
      fprintf fmt "possible duplicate families: %s and %s\n" (string_of_ifam f1)
        (string_of_ifam f2)
  | PossibleDuplicateFamHomonymous (f1, f2, p) ->
      let f = foi base f1 in
      let fath = get_father f in
      let moth = get_mother f in
      let curr, hom =
        if eq_iper fath (get_iper p) then (moth, fath) else (fath, moth)
      in
      fprintf fmt
        "possible duplicate families: %s and %s, %s has unions with several \
         persons named %s\n"
        (string_of_ifam f1) (string_of_ifam f2)
        (designation base (poi base curr))
        (designation base (poi base hom))
  | PEventOrder (p, e1, e2) ->
      fprintf fmt "%s's %s before his/her %s\n" (designation base p)
        (string_of_epers_name base (get_pevent_name e1))
        (string_of_epers_name base (get_pevent_name e2))
  | PWitnessEventAfterDeath (p, e, _origin) ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "witnessed the %s after his/her death\n"
        (string_of_epers_name base (get_pevent_name e))
  | PWitnessEventBeforeBirth (p, e, _origin) ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "witnessed the %s before his/her birth\n"
        (string_of_epers_name base (get_pevent_name e))
  | TitleDatesError (p, t) ->
      fprintf fmt "%s " (designation base p);
      fprintf fmt "has incorrect title dates as:\n";
      fprintf fmt " %s %s\n" (sou base t.t_ident) (sou base t.t_place)
  | UndefinedSex p -> fprintf fmt "Undefined sex for %s\n" (designation base p)
  | YoungForMarriage (p, a, _) | OldForMarriage (p, a, _) ->
      fprintf fmt "%s married at age %d\n" (designation base p) a.year

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
  Option.map aux (Date.cdate_to_dmy_opt (get_birth p))

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
    let p = poi base ip in
    let new_year, new_ini_p =
      match min_year_of p with Some y -> (y, p) | None -> (infer year, ini_p)
    in
    Gwdb.Marker.set year_tab ip new_year;
    test new_year year (lazy p) ini_p;
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        let f ip =
          let year = Gwdb.Marker.get year_tab ip in
          if year = dummy_date then
            check_ancestors base warning new_year year_tab ip new_ini_p
          else if own year then
            test year new_year (lazy (poi base ip)) new_ini_p
        in
        f @@ get_father fam;
        f @@ get_mother fam
    | None -> ())

let check_base ?(verbose = false) ?(mem = false) base error warning =
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
        let p = poi base ip in
        if Gwdb.Marker.get year_tab ip = dummy_date then
          check_ancestors base warning dummy_date year_tab ip p;
        CheckItem.person ~onchange:false base warning p)
      persons;
    ProgrBar.finish ())
  else
    Gwdb.Collection.iter
      (fun ip ->
        let p = poi base ip in
        if Gwdb.Marker.get year_tab ip = dummy_date then
          check_ancestors base warning dummy_date year_tab ip p;
        CheckItem.person ~onchange:false base warning p)
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
        CheckItem.family ~onchange:false base warning ifam @@ foi base ifam)
      families;
    ProgrBar.finish ())
  else
    Gwdb.Collection.iter
      (fun ifam ->
        CheckItem.family ~onchange:false base warning ifam @@ foi base ifam)
      families;
  if not mem then (
    Gwdb.clear_persons_array base;
    Gwdb.clear_families_array base;
    Gwdb.clear_descends_array base);
  Consang.check_noloop base error;
  if not mem then (
    Gwdb.clear_ascends_array base;
    Gwdb.clear_couples_array base)
