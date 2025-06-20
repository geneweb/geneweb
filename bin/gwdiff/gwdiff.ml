(* Copyright (c) 2001 Ludovic LEDIEU *)

open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

(*= TODO =====================================================================
  - Improve the way not to check several time the same persons.
=========================================================================== *)

let in_file1 = ref ""
let in_file2 = ref ""
let html = ref false
let root = ref ""
let cr = ref ""

(** Messages are printed when there is a difference between a person present in
    the two bases explored. *)
type messages =
  | MsgBadChild of Driver.iper
  | MsgBirthDate
  | MsgBirthPlace
  | MsgChildMissing of Driver.iper
  | MsgChildren of Driver.iper
  | MsgDeathDate
  | MsgDeathPlace
  | MsgDivorce
  | MsgFirstName
  | MsgOccupation
  | MsgParentsMissing
  | MsgMarriageDate
  | MsgMarriagePlace
  | MsgSex
  | MsgSpouseMissing of Driver.iper
  | MsgSpouses of Driver.iper
  | MsgSurname

(** [person_string base iper] Returns the string associated to the person with
    id `iper` in the base `base`. *)
let person_string base iper =
  let p = Driver.poi base iper in
  let fn = Driver.sou base (Driver.get_first_name p) in
  let sn = Driver.sou base (Driver.get_surname p) in
  if sn = "?" || fn = "?" then
    fn ^ " " ^ sn ^ " (#" ^ Driver.Iper.to_string iper ^ ")"
  else fn ^ "." ^ string_of_int (Driver.get_occ p) ^ " " ^ sn

(** Returns the string associated to a person in HTML if the html option is set,
    otherwise it has the same effect tja, `person_string`. *)
let person_link bname base iper target =
  if !html then
    Printf.sprintf "<A HREF=\"%s%s_w?i=%s\" TARGET=\"%s\">%s</A>" !root bname
      (Driver.Iper.to_string iper)
      target (person_string base iper)
  else person_string base iper

(** Prints a message *)
let print_message base1 msg =
  Printf.printf " ";
  (match msg with
  | MsgBadChild iper1 ->
      Printf.printf "can not isolate one child match: %s"
        (person_link !in_file1 base1 iper1 "base1")
  | MsgBirthDate -> Printf.printf "birth date"
  | MsgBirthPlace -> Printf.printf "birth place"
  | MsgChildMissing iper1 ->
      Printf.printf "child missing: %s"
        (person_link !in_file1 base1 iper1 "base1")
  | MsgChildren iper1 ->
      Printf.printf "more than one child match: %s"
        (person_link !in_file1 base1 iper1 "base1")
  | MsgDeathDate -> Printf.printf "death (status or date)"
  | MsgDeathPlace -> Printf.printf "death place"
  | MsgDivorce -> Printf.printf "divorce"
  | MsgFirstName -> Printf.printf "first name"
  | MsgOccupation -> Printf.printf "occupation"
  | MsgParentsMissing -> Printf.printf "parents missing"
  | MsgMarriageDate -> Printf.printf "marriage date"
  | MsgMarriagePlace -> Printf.printf "marriage place"
  | MsgSex -> Printf.printf "sex"
  | MsgSpouseMissing iper1 ->
      Printf.printf "spouse missing: %s"
        (person_link !in_file1 base1 iper1 "base1")
  | MsgSpouses iper1 ->
      Printf.printf "more than one spouse match: %s"
        (person_link !in_file1 base1 iper1 "base1")
  | MsgSurname -> Printf.printf "surname");
  Printf.printf "%s" !cr

(** Prints messages associates to the two families identifiers in argument *)
let print_f_messages base1 base2 ifam1 ifam2 res =
  let f1 = Driver.foi base1 ifam1 in
  let f2 = Driver.foi base2 ifam2 in
  Printf.printf "%s x %s%s/ %s x %s%s"
    (person_link !in_file1 base1 (Driver.get_father f1) "base1")
    (person_link !in_file1 base1 (Driver.get_mother f1) "base1")
    !cr
    (person_link !in_file2 base2 (Driver.get_father f2) "base2")
    (person_link !in_file2 base2 (Driver.get_father f2) "base2")
    !cr;
  List.iter (print_message base1) res

(** Same, but for persons *)
let print_p_messages base1 base2 iper1 iper2 res =
  Printf.printf "%s / %s%s"
    (person_link !in_file1 base1 iper1 "base1")
    (person_link !in_file2 base2 iper2 "base2")
    !cr;
  List.iter (print_message base1) res

(** [compatible_names src_name dest_name_list] Returns true if `src_name` is in
    `dest_name_list` (case insensitive) *)
let compatible_names src_name dest_name_list =
  let src_name = Name.lower src_name in
  let dest_name_list = List.map Name.lower dest_name_list in
  List.mem src_name dest_name_list

(** [compatible_str_field istr1 istr2] Checks the compatibility of two string
    identifiers, i.e. if istr1 is not the empty string identifier, then istr2
    must not be. *)
let compatible_str_field istr1 istr2 =
  Driver.Istr.is_empty istr1 || not (Driver.Istr.is_empty istr2)

(** Returns a list of intervals of SDN (SDN 1 is November 25, 4714 BC Gregorian
    calendar) of the date in argument. An interval has the format (b, b'), where
    b is an optional lower bound (None => no bound), and b' an optional upper
    bound. *)
let dmy_to_sdn_range_l dmy =
  let sdn_of_dmy dmy =
    let sdn = Calendar.sdn_of_gregorian dmy in
    let sdn = if dmy.month = 0 || dmy.day = 0 then sdn + 1 else sdn in
    let sdn2 =
      if dmy.delta != 0 then sdn + dmy.delta
      else
        let dmy2 =
          {
            year =
              (if dmy.month = 0 || (dmy.month = 12 && dmy.day = 0) then
                 dmy.year + 1
               else dmy.year);
            month =
              (if dmy.month = 0 then 1
               else if dmy.day = 0 then
                 if dmy.month = 12 then 1 else dmy.month + 1
               else dmy.month);
            day = (if dmy.day = 0 then 1 else dmy.day);
            prec = (if dmy.month = 0 || dmy.day = 0 then Before else Sure);
            delta = dmy.delta;
          }
        in
        let sdn2 = Calendar.sdn_of_gregorian dmy2 in
        if dmy2.prec = Before then sdn2 - 1 else sdn2
    in
    (sdn, sdn2)
  in
  (* S: calls to sdn_of_dmy dmy can be factorized *)
  match dmy.prec with
  | Sure ->
      let sdn1, sdn2 = sdn_of_dmy dmy in
      [ (Some sdn1, Some sdn2) ]
  | Maybe ->
      let sdn1, sdn2 = sdn_of_dmy dmy in
      [ (Some sdn1, Some sdn2); (None, None) ]
  | About ->
      let sdn1, sdn2 = sdn_of_dmy dmy in
      let delta = (sdn2 - sdn1 + 1) * 5 in
      [ (Some (sdn1 - delta), Some (sdn2 + delta)) ]
  | Before ->
      let _sdn1, sdn2 = sdn_of_dmy dmy in
      [ (None, Some sdn2) ]
  | After ->
      let sdn1, _sdn2 = sdn_of_dmy dmy in
      [ (Some sdn1, None) ]
  | OrYear dmy2 ->
      let sdn11, sdn12 = sdn_of_dmy dmy in
      let sdn21, sdn22 = sdn_of_dmy (Date.dmy_of_dmy2 dmy2) in
      [ (Some sdn11, Some sdn12); (Some sdn21, Some sdn22) ]
  | YearInt dmy2 ->
      let sdn11, _sdn12 = sdn_of_dmy dmy in
      let _sdn21, sdn22 = sdn_of_dmy (Date.dmy_of_dmy2 dmy2) in
      [ (Some sdn11, Some sdn22) ]

(** [compatible_sdn i1 i2] Checks if two intervals `i1` and `i2` (as described
    for `dmy_to_sdn_range_l`) are compatible, i.e. if i2 is a sub interval of
    i1. *)
let compatible_sdn (sdn11, sdn12) (sdn21, sdn22) =
  if (sdn21, sdn22) = (None, None) then true
  else
    (* S: Add unit argument to bool2 to make good use of OCaml laziness  *)
    let bool1 =
      match (sdn11, sdn21) with
      | Some sdn1, Some sdn2 -> sdn1 <= sdn2
      | None, _ -> true
      | Some _, None -> false
    in
    let bool2 =
      match (sdn12, sdn22) with
      | Some sdn1, Some sdn2 -> sdn1 >= sdn2
      | None, _ -> true
      | Some _, None -> false
    in
    bool1 && bool2

(** [compatible_sdn_l l i] Checks if there exists an interval in `l` that is
    compatible with `i` *)
let compatible_sdn_l sdn1_l sdn2 =
  (* S: replace by List.exists *)
  List.fold_left (fun r sdn1 -> r || compatible_sdn sdn1 sdn2) false sdn1_l

(** [compatible_sdn_l l1 l2] Checks if for all intervals `i2` in `l2`, there
    exists an interval `i1` in `l1` such that `i1` is compatible with `i2` *)
let compatible_sdn_ll sdn1_l sdn2_l =
  List.fold_left (fun r sdn2 -> r && compatible_sdn_l sdn1_l sdn2) true sdn2_l

(** [compatible_dmys d1 d2] Checks if `d1` is compatible with `d2`, i.e. if
    despite a potential lack of precision in the dates, d2 is more precise than
    d1. *)
let compatible_dmys dmy1 dmy2 =
  compatible_sdn_ll (dmy_to_sdn_range_l dmy1) (dmy_to_sdn_range_l dmy2)

(** [compatible_dates date1 date2] Same than before, but also checks the kind of
    date (Dgreg or Dtext) and, in the first case, if calendars are compatible.
*)
let compatible_dates date1 date2 =
  let compatible_cals cal1 cal2 =
    match (cal1, cal2) with
    | Dgregorian, Djulian | Dgregorian, Dfrench -> true
    | _ -> cal1 = cal2
  in
  if date1 = date2 then true
  else
    match (date1, date2) with
    | Dgreg (dmy1, cal1), Dgreg (dmy2, cal2) ->
        compatible_dmys dmy1 dmy2 && compatible_cals cal1 cal2
    | Dgreg (_, _), Dtext _ -> false
    | Dtext _, _ -> true

(** Same than before, but for Adef.ctype. *)
let compatible_cdates cdate1 cdate2 =
  let od1 = Date.od_of_cdate cdate1 in
  let od2 = Date.od_of_cdate cdate2 in
  match (od1, od2) with
  | Some date1, Some date2 -> compatible_dates date1 date2
  | Some _, None -> false
  | None, _ -> true

(** Checks if birth between two persons are compatible, i.e. if their birth date
    (baptism date if birth date not provided) and place are compatible, and
    returns a list of messages. If birth is not provided, checks bathism date
    instead. If birth/bathism date are not compatible, the returned list will
    have MsgBirthDate If birth place are not compatible, the returned list will
    have MsgBirthPlace *)
let compatible_birth p1 p2 =
  let get_birth person =
    if person.birth = Date.cdate_None then person.baptism else person.birth
  in
  let birth1 = get_birth p1 in
  let birth2 = get_birth p2 in
  let res1 = if compatible_cdates birth1 birth2 then [] else [ MsgBirthDate ] in
  let res2 =
    if compatible_str_field p1.birth_place p2.birth_place then []
    else [ MsgBirthPlace ]
  in
  res1 @ res2

(** Same than before, but for death. Messages returned are MsgDeathDate and
    MsgDeathPlace *)
let compatible_death p1 p2 =
  let bool1 =
    p1.death = p2.death
    ||
    match (p1.death, p2.death) with
    | Death (_, cdate1), Death (_, cdate2) ->
        let date1 = Date.date_of_cdate cdate1 in
        let date2 = Date.date_of_cdate cdate2 in
        compatible_dates date1 date2
    | NotDead, _
    | DeadYoung, Death (_, _)
    | DeadDontKnowWhen, (Death (_, _) | DeadYoung | DeadDontKnowWhen)
    | DontKnowIfDead, _ ->
        true
    | _ -> (* S: avoid non-exhaustive pattern matching *) false
  in
  let res1 = if bool1 then [] else [ MsgDeathDate ] in
  let res2 =
    if compatible_str_field p1.death_place p2.death_place then []
    else [ MsgDeathPlace ]
  in
  res1 @ res2

(** [compatible_sexes p1 p2] Returns [] if `p1` and `p2` have the same sex,
    [MsgSex] otherwise. *)
let compatible_sexes p1 p2 = if p1.sex = p2.sex then [] else [ MsgSex ]

(** [compatible_occupations p1 p2] Returns [] if `p1` and `p2` have compatible
    occupations, [MsgOccupation] otherwise. *)
let compatible_occupations p1 p2 =
  if compatible_str_field p1.occupation p2.occupation then []
  else [ MsgOccupation ]

(** Checks if two persons' names are compatible wrt. their eventual aliases and
    returns a list of messages. If first names are not compatible, the returned
    list will have MsgFirstName. If surnames are not compatible, the returned
    list will have MsgSurname. *)
let compatible_persons_ligth base1 base2 p1 p2 =
  let fn1 = Driver.sou base1 p1.first_name in
  let fn2 = Driver.sou base2 p2.first_name in
  let afn2 = fn2 :: List.map (Driver.sou base2) p2.first_names_aliases in
  let sn1 = Driver.sou base1 p1.surname in
  let sn2 = Driver.sou base2 p2.surname in
  let asn2 = sn2 :: List.map (Driver.sou base2) p2.surnames_aliases in
  let res1 = if compatible_names fn1 afn2 then [] else [ MsgFirstName ] in
  let res2 = if compatible_names sn1 asn2 then [] else [ MsgSurname ] in
  res1 @ res2

(** Checks if two persons are compatible and returns all the messages associated
    to the compatiblity of their name, sex, birth, death and occupation. *)
let compatible_persons base1 base2 p1 p2 =
  compatible_persons_ligth base1 base2 p1 p2
  @ compatible_sexes p1 p2 @ compatible_birth p1 p2 @ compatible_death p1 p2
  @ compatible_occupations p1 p2

(** [find_compatible_persons_ligth base1 base2 iper1 iper2_list] Returns the
    sublist of persons of `iper2_list` that are compatible with `iper1` (only
    checking names). *)
let rec find_compatible_persons_ligth base1 base2 iper1 iper2_list =
  (* S: not tail recursive, could be *)
  match iper2_list with
  | [] -> []
  | head :: rest ->
      let p1 = Driver.gen_person_of_person (Driver.poi base1 iper1) in
      let p2 = Driver.gen_person_of_person (Driver.poi base2 head) in
      let c_rest = find_compatible_persons_ligth base1 base2 iper1 rest in
      if compatible_persons_ligth base1 base2 p1 p2 = [] then head :: c_rest
      else c_rest

(** Same than before, but with full compatibility ( name, sex, birth, death and
    occupation) *)
let rec find_compatible_persons base1 base2 iper1 iper2_list =
  match iper2_list with
  | [] -> []
  | head :: rest ->
      let p1 = Driver.gen_person_of_person (Driver.poi base1 iper1) in
      let p2 = Driver.gen_person_of_person (Driver.poi base2 head) in
      let c_rest = find_compatible_persons base1 base2 iper1 rest in
      if compatible_persons base1 base2 p1 p2 = [] then head :: c_rest
      else c_rest

(** Checks if the spouse of the persons (whose id are in argument) are
    compatible (only checking names) and returns the associated messages list.
*)
let compatible_unions base1 base2 iper1 iper2 ifam1 ifam2 =
  let get_spouse base iper ifam =
    let f = Driver.foi base ifam in
    if iper = Driver.get_father f then Driver.poi base (Driver.get_mother f)
    else Driver.poi base (Driver.get_father f)
  in
  let spouse1 = Driver.gen_person_of_person (get_spouse base1 iper1 ifam1) in
  let spouse2 = Driver.gen_person_of_person (get_spouse base2 iper2 ifam2) in
  compatible_persons_ligth base1 base2 spouse1 spouse2

(** [find_compatible_unions base1 base2 iper1 iper2_list ifam1 ifam2_list]
    Returns the sublist of families of `ifam2_list` whose union is compatible
    (in the sense of `compatible_unions`). *)
let rec find_compatible_unions base1 base2 iper1 iper2 ifam1 ifam2_list =
  match ifam2_list with
  | [] -> []
  | head :: rest ->
      let c_rest = find_compatible_unions base1 base2 iper1 iper2 ifam1 rest in
      if compatible_unions base1 base2 iper1 iper2 ifam1 head = [] then
        head :: c_rest
      else c_rest

(** [compatible_divorces d1 d2] Returns true if divorces are compatible, i.e. if
    both divorced, then checking date compatibility, if d1 is a divorce and d2
    is not returns false, otherwise returns true. *)
let compatible_divorces d1 d2 =
  match (d1, d2) with
  | Divorced cdate1, Divorced cdate2 -> compatible_cdates cdate1 cdate2
  | Divorced _, _ -> false
  | _ -> true

(** Checks the compatibility of marriages (mariage date, divorce and mariage
    place), then print the list of messages calculated. *)
let compatible_marriages base1 base2 ifam1 ifam2 =
  let f1 = Driver.gen_family_of_family (Driver.foi base1 ifam1) in
  let f2 = Driver.gen_family_of_family (Driver.foi base2 ifam2) in
  let res1 =
    if compatible_cdates f1.marriage f2.marriage then []
    else [ MsgMarriageDate ]
  in
  let res2 =
    if compatible_divorces f1.divorce f2.divorce then [] else [ MsgDivorce ]
  in
  let res3 =
    if compatible_str_field f1.marriage_place f2.marriage_place then []
    else [ MsgMarriagePlace ]
  in
  let res = res1 @ res2 @ res3 in
  if res = [] then () else print_f_messages base1 base2 ifam1 ifam2 res

(** Calculates the compatibility of two persons and prints the associated
    messages *)
let pdiff base1 base2 iper1 iper2 =
  let p1 = Driver.gen_person_of_person (Driver.poi base1 iper1) in
  let p2 = Driver.gen_person_of_person (Driver.poi base2 iper2) in
  let res = compatible_persons base1 base2 p1 p2 in
  if res = [] then () else print_p_messages base1 base2 iper1 iper2 res

(** Calculates the compatibility of two persons' families and prints the
    associated messages. *)
let compatible_parents base1 base2 iper1 iper2 =
  let a1 = Driver.get_parents (Driver.poi base1 iper1) in
  let a2 = Driver.get_parents (Driver.poi base2 iper2) in
  match (a1, a2) with
  | Some ifam1, Some ifam2 ->
      let f1 = Driver.foi base1 ifam1 in
      let f2 = Driver.foi base2 ifam2 in
      let _ = pdiff base1 base2 (Driver.get_father f1) (Driver.get_father f2) in
      let _ = pdiff base1 base2 (Driver.get_mother f1) (Driver.get_mother f2) in
      compatible_marriages base1 base2 ifam1 ifam2
  | None, _ -> ()
  | Some _, None ->
      print_p_messages base1 base2 iper1 iper2 [ MsgParentsMissing ]

(** Checks che compatibility of two persons and their families, and prints it.
    This is performed recursively through their descendants *)
let rec ddiff base1 base2 iper1 iper2 d_tab =
  (* S: Simplify with statement:
     let ddiff iper1 iper2 = ddiff base1 base2 iper1 iper2 d_tab *)
  let d_check = Collection.Marker.get d_tab iper1 in
  if List.mem iper2 d_check then ()
  else Collection.Marker.set d_tab iper1 (iper2 :: d_check);
  let spouse f iper =
    if iper = Driver.get_father f then Driver.get_mother f
    else Driver.get_father f
  in
  let udiff base1 base2 iper1 iper2 ifam1 ifam2 =
    let fd b1 b2 ip2_list ip1 =
      match find_compatible_persons_ligth b1 b2 ip1 ip2_list with
      | [ ip2 ] -> ddiff base1 base2 ip1 ip2 d_tab
      | [] -> print_p_messages base1 base2 iper1 iper2 [ MsgChildMissing ip1 ]
      | rest_list -> (
          match find_compatible_persons b1 b2 ip1 rest_list with
          | [ best_ip2 ] -> ddiff base1 base2 ip1 best_ip2 d_tab
          | [] -> print_p_messages base1 base2 iper1 iper2 [ MsgBadChild ip1 ]
          | _ -> print_p_messages base1 base2 iper1 iper2 [ MsgChildren ip1 ])
    in
    let f1 = Driver.foi base1 ifam1 in
    let f2 = Driver.foi base2 ifam2 in
    let p1 = spouse f1 iper1 in
    let p2 = spouse f2 iper2 in
    let d1 = Array.to_list (Driver.get_children (Driver.foi base1 ifam1)) in
    let d2 = Array.to_list (Driver.get_children (Driver.foi base2 ifam2)) in
    pdiff base1 base2 p1 p2;
    List.iter (fd base1 base2 d2) d1
  in
  let fu b1 b2 ifam2_list ifam1 =
    match find_compatible_unions b1 b2 iper1 iper2 ifam1 ifam2_list with
    | [ ifam2 ] ->
        compatible_marriages b1 b2 ifam1 ifam2;
        compatible_parents b1 b2
          (spouse (Driver.foi base1 ifam1) iper1)
          (spouse (Driver.foi base2 ifam2) iper2);
        udiff b1 b2 iper1 iper2 ifam1 ifam2
    | [] ->
        print_p_messages base1 base2 iper1 iper2
          [ MsgSpouseMissing (spouse (Driver.foi base1 ifam1) iper1) ]
    | _ ->
        print_p_messages base1 base2 iper1 iper2
          [ MsgSpouses (spouse (Driver.foi base1 ifam1) iper1) ]
  in
  let u1 = Array.to_list (Driver.get_family (Driver.poi base1 iper1)) in
  let u2 = Array.to_list (Driver.get_family (Driver.poi base2 iper2)) in
  pdiff base1 base2 iper1 iper2;
  List.iter (fu base1 base2 u2) u1

(** Returns the eldest persons on the base starting from the persons in
    argument. *)
let rec find_top base1 base2 iper1 iper2 =
  let p1 = Driver.gen_person_of_person (Driver.poi base1 iper1) in
  let p2 = Driver.gen_person_of_person (Driver.poi base2 iper2) in
  if compatible_persons_ligth base1 base2 p1 p2 = [] then
    let a1 = Driver.get_parents (Driver.poi base1 iper1) in
    let a2 = Driver.get_parents (Driver.poi base2 iper2) in
    match (a1, a2) with
    | Some ifam1, Some ifam2 ->
        let f1 = Driver.foi base1 ifam1 in
        let f2 = Driver.foi base2 ifam2 in
        let f_top_list =
          find_top base1 base2 (Driver.get_father f1) (Driver.get_father f2)
        in
        let m_top_list =
          find_top base1 base2 (Driver.get_mother f1) (Driver.get_mother f2)
        in
        f_top_list @ m_top_list
    | _ -> [ (iper1, iper2) ]
  else (
    Printf.printf " Warning: %s doesn't match %s%s"
      (person_link !in_file1 base1 iper1 "base1")
      (person_link !in_file2 base2 iper2 "base2")
      !cr;
    [])

(** Same than ddiff, but starting from the eldest ancestors from the persons in
    argument *)
let addiff base1 base2 iper1 iper2 d_tab =
  let topdiff (iper1, iper2) =
    Printf.printf "==> %s / %s%s"
      (person_link !in_file1 base1 iper1 "base1")
      (person_link !in_file2 base2 iper2 "base2")
      !cr;
    ddiff base1 base2 iper1 iper2 d_tab
  in
  Printf.printf "Building top list...%s" !cr;
  let top_list = find_top base1 base2 iper1 iper2 in
  Printf.printf "Top list built.%s" !cr;
  List.iter topdiff top_list

(* Main *)

let gwdiff base1 base2 iper1 iper2 d_mode ad_mode =
  let desc_tab =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base1) []
  in
  match (d_mode, ad_mode) with
  | true, _ | false, false -> ddiff base1 base2 iper1 iper2 desc_tab
  | false, true -> addiff base1 base2 iper1 iper2 desc_tab

let p1_fn = ref ""
let p1_occ = ref 0
let p1_sn = ref ""
let p2_fn = ref ""
let p2_occ = ref 0
let p2_sn = ref ""

type arg_state = ASnone | ASwaitP1occ | ASwaitP1sn | ASwaitP2occ | ASwaitP2sn

let arg_state = ref ASnone
let mem = ref false
let d_mode = ref false
let ad_mode = ref false

let speclist =
  [
    ( "-1",
      Arg.String
        (fun s ->
          p1_fn := s;
          arg_state := ASwaitP1occ),
      "<fn> <occ> <sn> (mandatory) defines starting person in base1" );
    ( "-2",
      Arg.String
        (fun s ->
          p2_fn := s;
          arg_state := ASwaitP2occ),
      "<fn> <occ> <sn> (mandatory) defines starting person in base2" );
    ("-ad", Arg.Set ad_mode, " checks descendants of all ascendants ");
    ("-d", Arg.Set d_mode, " checks descendants (default)");
    ( "-html",
      Arg.String
        (fun s ->
          html := true;
          root := s),
      "<root> HTML format used for report" );
    ("-mem", Arg.Set mem, " save memory space, but slower");
  ]
  |> List.sort compare |> Arg.align

let anonfun s =
  match !arg_state with
  | ASnone ->
      if !in_file1 = "" then in_file1 := s
      else if !in_file2 = "" then in_file2 := s
      else raise (Arg.Bad "Too much arguments")
  | ASwaitP1occ -> (
      try
        p1_occ := int_of_string s;
        arg_state := ASwaitP1sn
      with Failure _ -> raise (Arg.Bad "Numeric value for occ (-1)!"))
  | ASwaitP1sn ->
      p1_sn := s;
      arg_state := ASnone
  | ASwaitP2occ -> (
      try
        p2_occ := int_of_string s;
        arg_state := ASwaitP2sn
      with Failure _ -> raise (Arg.Bad "Numeric value for occ (-2)!"))
  | ASwaitP2sn ->
      p2_sn := s;
      arg_state := ASnone

let errmsg = "Usage: " ^ Sys.argv.(0) ^ " [options] base1 base2\nOptions are: "

let check_args () =
  Arg.parse speclist anonfun errmsg;
  if !in_file1 = "" then (
    Printf.printf "Missing reference data base\n";
    Printf.printf "Use option -help for usage\n";
    flush stdout;
    exit 2);
  if !in_file2 = "" then (
    Printf.printf "Missing destination data base\n";
    Printf.printf "Use option -help for usage\n";
    flush stdout;
    exit 2);
  if !p1_fn = "" then (
    Printf.printf "-1 parameter is mandatory\n";
    Printf.printf "Use option -help for usage\n";
    flush stdout;
    exit 2);
  if !p1_sn = "" then (
    Printf.printf "Incomplete -1 parameter\n";
    Printf.printf "Use option -help for usage\n";
    flush stdout;
    exit 2);
  if !p2_fn = "" then (
    Printf.printf "-2 parameter is mandatory\n";
    Printf.printf "Use option -help for usage\n";
    flush stdout;
    exit 2);
  if !p2_sn = "" then (
    Printf.printf "Incomplete -2 parameter\n";
    Printf.printf "Use option -help for usage\n";
    flush stdout;
    exit 2)

let load_base f k =
  Driver.with_database f (fun base ->
      Driver.load_ascends_array base;
      Driver.load_strings_array base;
      if not !mem then (
        Driver.load_unions_array base;
        Driver.load_couples_array base;
        Driver.load_descends_array base);
      k base)

let main () =
  let _ = check_args () in
  let _ = if not !html then cr := "\n" else cr := "<BR>\n" in
  (* [base1] is the reference base and [base2] is the destination base. *)
  load_base !in_file1 @@ fun base1 ->
  load_base !in_file2 @@ fun base2 ->
  let iper1 = Driver.person_of_key base1 !p1_fn !p1_sn !p1_occ in
  let iper2 = Driver.person_of_key base2 !p2_fn !p2_sn !p2_occ in
  if !html then Printf.printf "<BODY>\n";
  (match (iper1, iper2) with
  | None, _ ->
      Printf.printf "Cannot find person %s.%d %s in reference base" !p1_fn
        !p1_occ !p1_sn
  | _, None ->
      Printf.printf "Cannot find person %s.%d %s in destination base" !p2_fn
        !p2_occ !p2_sn
  | Some iper1, Some iper2 -> gwdiff base1 base2 iper1 iper2 !d_mode !ad_mode);
  if !html then Printf.printf "</BODY>\n"

let _ = Printexc.print main ()
