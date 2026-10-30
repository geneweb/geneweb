open Def
module Driver = Geneweb_db.Driver

type person = { first_name : string; surname : string; occ : int }
type couple = { father : person; mother : person }
type pevent = string Def.gen_pers_event_name
type fevent = string Def.gen_fam_event_name

type t =
  | BigAgeBetweenSpouses of person * person * int
  | BirthAfterDeath of person
  | ChangedOrderOfChildren of couple
  | ChangedOrderOfFamilyEvents of couple
  | ChangedOrderOfMarriages of person
  | ChangedOrderOfPersonEvents of person
  | ChildrenNotInOrder of couple * person * person
  | CloseChildren of couple * person * person
  | DeadOld of person * int
  | DeadTooEarlyToBeFather of person * person
  | DistantChildren of couple * person * person
  | FEventOrder of person * fevent * fevent
  | FWitnessEventAfterDeath of person * fevent * couple
  | FWitnessEventBeforeBirth of person * fevent * couple
  | IncoherentAncestorDate of person * person
  | IncoherentSex of person
  | MarriageDateAfterDeath of person
  | MarriageDateBeforeBirth of person
  | MotherDeadBeforeChildBirth of person * person
  | OldForMarriage of person * int * couple
  | ParentBornAfterChild of person * person
  | ParentTooOld of person * int * person
  | ParentTooYoung of person * int * person
  | PEventOrder of person * pevent * pevent
  | PossibleDuplicateFam of couple
  | PossibleDuplicateFamHomonymous of couple * person
  | PWitnessEventAfterDeath of person * pevent * person
  | PWitnessEventBeforeBirth of person * pevent * person
  | TitleDatesError of person * string
  | UndefinedSex of person
  | YoungForMarriage of person * int * couple

type kind =
  | K_BigAgeBetweenSpouses
  | K_BirthAfterDeath
  | K_ChangedOrderOfChildren
  | K_ChangedOrderOfFamilyEvents
  | K_ChangedOrderOfMarriages
  | K_ChangedOrderOfPersonEvents
  | K_ChildrenNotInOrder
  | K_CloseChildren
  | K_DeadOld
  | K_DeadTooEarlyToBeFather
  | K_DistantChildren
  | K_FEventOrder
  | K_FWitnessEventAfterDeath
  | K_FWitnessEventBeforeBirth
  | K_IncoherentAncestorDate
  | K_IncoherentSex
  | K_MarriageDateAfterDeath
  | K_MarriageDateBeforeBirth
  | K_MotherDeadBeforeChildBirth
  | K_OldForMarriage
  | K_ParentBornAfterChild
  | K_ParentTooOld
  | K_ParentTooYoung
  | K_PEventOrder
  | K_PossibleDuplicateFam
  | K_PossibleDuplicateFamHomonymous
  | K_PWitnessEventAfterDeath
  | K_PWitnessEventBeforeBirth
  | K_TitleDatesError
  | K_UndefinedSex
  | K_YoungForMarriage

let kind a =
  match a with
  | BigAgeBetweenSpouses _ -> K_BigAgeBetweenSpouses
  | BirthAfterDeath _ -> K_BirthAfterDeath
  | ChangedOrderOfChildren _ -> K_ChangedOrderOfChildren
  | ChangedOrderOfFamilyEvents _ -> K_ChangedOrderOfFamilyEvents
  | ChangedOrderOfMarriages _ -> K_ChangedOrderOfMarriages
  | ChangedOrderOfPersonEvents _ -> K_ChangedOrderOfPersonEvents
  | ChildrenNotInOrder _ -> K_ChildrenNotInOrder
  | CloseChildren _ -> K_CloseChildren
  | DeadOld _ -> K_DeadOld
  | DeadTooEarlyToBeFather _ -> K_DeadTooEarlyToBeFather
  | DistantChildren _ -> K_DistantChildren
  | FEventOrder _ -> K_FEventOrder
  | FWitnessEventAfterDeath _ -> K_FWitnessEventAfterDeath
  | FWitnessEventBeforeBirth _ -> K_FWitnessEventBeforeBirth
  | IncoherentAncestorDate _ -> K_IncoherentAncestorDate
  | IncoherentSex _ -> K_IncoherentSex
  | MarriageDateAfterDeath _ -> K_MarriageDateAfterDeath
  | MarriageDateBeforeBirth _ -> K_MarriageDateBeforeBirth
  | MotherDeadBeforeChildBirth _ -> K_MotherDeadBeforeChildBirth
  | OldForMarriage _ -> K_OldForMarriage
  | ParentBornAfterChild _ -> K_ParentBornAfterChild
  | ParentTooOld _ -> K_ParentTooOld
  | ParentTooYoung _ -> K_ParentTooYoung
  | PEventOrder _ -> K_PEventOrder
  | PossibleDuplicateFam _ -> K_PossibleDuplicateFam
  | PossibleDuplicateFamHomonymous _ -> K_PossibleDuplicateFamHomonymous
  | PWitnessEventAfterDeath _ -> K_PWitnessEventAfterDeath
  | PWitnessEventBeforeBirth _ -> K_PWitnessEventBeforeBirth
  | TitleDatesError _ -> K_TitleDatesError
  | UndefinedSex _ -> K_UndefinedSex
  | YoungForMarriage _ -> K_YoungForMarriage

(* Display order: dates impossible first, then suspicious ages, then the
   checks that are mostly about data entry (order, sex, duplicates). *)
let all_kinds =
  [
    K_BirthAfterDeath;
    K_ParentBornAfterChild;
    K_IncoherentAncestorDate;
    K_MotherDeadBeforeChildBirth;
    K_DeadTooEarlyToBeFather;
    K_MarriageDateBeforeBirth;
    K_MarriageDateAfterDeath;
    K_PWitnessEventBeforeBirth;
    K_PWitnessEventAfterDeath;
    K_FWitnessEventBeforeBirth;
    K_FWitnessEventAfterDeath;
    K_TitleDatesError;
    K_YoungForMarriage;
    K_OldForMarriage;
    K_ParentTooYoung;
    K_ParentTooOld;
    K_DeadOld;
    K_BigAgeBetweenSpouses;
    K_CloseChildren;
    K_DistantChildren;
    K_ChildrenNotInOrder;
    K_ChangedOrderOfChildren;
    K_ChangedOrderOfMarriages;
    K_ChangedOrderOfFamilyEvents;
    K_ChangedOrderOfPersonEvents;
    K_PEventOrder;
    K_FEventOrder;
    K_IncoherentSex;
    K_UndefinedSex;
    K_PossibleDuplicateFam;
    K_PossibleDuplicateFamHomonymous;
  ]

let kind_name k =
  match k with
  | K_BigAgeBetweenSpouses -> "BigAgeBetweenSpouses"
  | K_BirthAfterDeath -> "BirthAfterDeath"
  | K_ChangedOrderOfChildren -> "ChangedOrderOfChildren"
  | K_ChangedOrderOfFamilyEvents -> "ChangedOrderOfFamilyEvents"
  | K_ChangedOrderOfMarriages -> "ChangedOrderOfMarriages"
  | K_ChangedOrderOfPersonEvents -> "ChangedOrderOfPersonEvents"
  | K_ChildrenNotInOrder -> "ChildrenNotInOrder"
  | K_CloseChildren -> "CloseChildren"
  | K_DeadOld -> "DeadOld"
  | K_DeadTooEarlyToBeFather -> "DeadTooEarlyToBeFather"
  | K_DistantChildren -> "DistantChildren"
  | K_FEventOrder -> "FEventOrder"
  | K_FWitnessEventAfterDeath -> "FWitnessEventAfterDeath"
  | K_FWitnessEventBeforeBirth -> "FWitnessEventBeforeBirth"
  | K_IncoherentAncestorDate -> "IncoherentAncestorDate"
  | K_IncoherentSex -> "IncoherentSex"
  | K_MarriageDateAfterDeath -> "MarriageDateAfterDeath"
  | K_MarriageDateBeforeBirth -> "MarriageDateBeforeBirth"
  | K_MotherDeadBeforeChildBirth -> "MotherDeadBeforeChildBirth"
  | K_OldForMarriage -> "OldForMarriage"
  | K_ParentBornAfterChild -> "ParentBornAfterChild"
  | K_ParentTooOld -> "ParentTooOld"
  | K_ParentTooYoung -> "ParentTooYoung"
  | K_PEventOrder -> "PEventOrder"
  | K_PossibleDuplicateFam -> "PossibleDuplicateFam"
  | K_PossibleDuplicateFamHomonymous -> "PossibleDuplicateFamHomonymous"
  | K_PWitnessEventAfterDeath -> "PWitnessEventAfterDeath"
  | K_PWitnessEventBeforeBirth -> "PWitnessEventBeforeBirth"
  | K_TitleDatesError -> "TitleDatesError"
  | K_UndefinedSex -> "UndefinedSex"
  | K_YoungForMarriage -> "YoungForMarriage"

let kind_of_name s = List.find_opt (fun k -> kind_name k = s) all_kinds

let age a =
  match a with
  | BigAgeBetweenSpouses (_, _, n)
  | DeadOld (_, n)
  | OldForMarriage (_, n, _)
  | ParentTooOld (_, n, _)
  | ParentTooYoung (_, n, _)
  | YoungForMarriage (_, n, _) ->
      Some n
  | BirthAfterDeath _ | ChangedOrderOfChildren _ | ChangedOrderOfFamilyEvents _
  | ChangedOrderOfMarriages _ | ChangedOrderOfPersonEvents _
  | ChildrenNotInOrder _ | CloseChildren _ | DeadTooEarlyToBeFather _
  | DistantChildren _ | FEventOrder _ | FWitnessEventAfterDeath _
  | FWitnessEventBeforeBirth _ | IncoherentAncestorDate _ | IncoherentSex _
  | MarriageDateAfterDeath _ | MarriageDateBeforeBirth _
  | MotherDeadBeforeChildBirth _ | ParentBornAfterChild _ | PEventOrder _
  | PossibleDuplicateFam _ | PossibleDuplicateFamHomonymous _
  | PWitnessEventAfterDeath _ | PWitnessEventBeforeBirth _ | TitleDatesError _
  | UndefinedSex _ ->
      None

type subject = Person of person | Couple of couple

let subjects a =
  match a with
  | BigAgeBetweenSpouses (p1, p2, _) -> [ Person p1; Person p2 ]
  | ChangedOrderOfChildren c
  | ChangedOrderOfFamilyEvents c
  | ChildrenNotInOrder (c, _, _)
  | CloseChildren (c, _, _)
  | DistantChildren (c, _, _)
  | PossibleDuplicateFam c
  | PossibleDuplicateFamHomonymous (c, _) ->
      [ Couple c ]
  | DeadTooEarlyToBeFather (_, child) | MotherDeadBeforeChildBirth (_, child) ->
      [ Person child ]
  | IncoherentAncestorDate (_, p) -> [ Person p ]
  | BirthAfterDeath p
  | ChangedOrderOfMarriages p
  | ChangedOrderOfPersonEvents p
  | DeadOld (p, _)
  | FEventOrder (p, _, _)
  | FWitnessEventAfterDeath (p, _, _)
  | FWitnessEventBeforeBirth (p, _, _)
  | IncoherentSex p
  | MarriageDateAfterDeath p
  | MarriageDateBeforeBirth p
  | OldForMarriage (p, _, _)
  | ParentBornAfterChild (p, _)
  | ParentTooOld (p, _, _)
  | ParentTooYoung (p, _, _)
  | PEventOrder (p, _, _)
  | PWitnessEventAfterDeath (p, _, _)
  | PWitnessEventBeforeBirth (p, _, _)
  | TitleDatesError (p, _)
  | UndefinedSex p
  | YoungForMarriage (p, _, _) ->
      [ Person p ]

let persons a =
  let of_couple c = [ c.father; c.mother ] in
  match a with
  | BigAgeBetweenSpouses (p1, p2, _)
  | DeadTooEarlyToBeFather (p1, p2)
  | IncoherentAncestorDate (p1, p2)
  | MotherDeadBeforeChildBirth (p1, p2) ->
      [ p2; p1 ]
  | ParentBornAfterChild (p1, p2)
  | ParentTooOld (p1, _, p2)
  | ParentTooYoung (p1, _, p2)
  | PWitnessEventAfterDeath (p1, _, p2)
  | PWitnessEventBeforeBirth (p1, _, p2) ->
      [ p1; p2 ]
  | ChangedOrderOfChildren c
  | ChangedOrderOfFamilyEvents c
  | PossibleDuplicateFam c ->
      of_couple c
  | ChildrenNotInOrder (c, p1, p2)
  | CloseChildren (c, p1, p2)
  | DistantChildren (c, p1, p2) ->
      c.father :: c.mother :: [ p1; p2 ]
  | PossibleDuplicateFamHomonymous (c, p) -> c.father :: c.mother :: [ p ]
  | FWitnessEventAfterDeath (p, _, c)
  | FWitnessEventBeforeBirth (p, _, c)
  | OldForMarriage (p, _, c)
  | YoungForMarriage (p, _, c) ->
      p :: of_couple c
  | BirthAfterDeath p
  | ChangedOrderOfMarriages p
  | ChangedOrderOfPersonEvents p
  | DeadOld (p, _)
  | FEventOrder (p, _, _)
  | IncoherentSex p
  | MarriageDateAfterDeath p
  | MarriageDateBeforeBirth p
  | PEventOrder (p, _, _)
  | TitleDatesError (p, _)
  | UndefinedSex p ->
      [ p ]

let person_key p = Printf.sprintf "%s.%d %s" p.first_name p.occ p.surname
let is_unnamed p = p.first_name = "?" || p.surname = "?"

(* ---- collecting ---------------------------------------------------------- *)

let person_of base p =
  {
    first_name = Driver.p_first_name base p;
    surname = Driver.p_surname base p;
    occ = Driver.get_occ p;
  }

let person_of_iper base ip = person_of base (Driver.poi base ip)

let couple_of base ifam =
  let fam = Driver.foi base ifam in
  {
    father = person_of_iper base (Driver.get_father fam);
    mother = person_of_iper base (Driver.get_mother fam);
  }

let pevent_of base (e : Driver.pers_event) =
  (Futil.map_pers_event Fun.id (Driver.sou base) e).epers_name

let fevent_of base (e : Driver.fam_event) =
  (Futil.map_fam_event Fun.id (Driver.sou base) e).efam_name

let of_warning base (w : CheckItem.base_warning) =
  let pers = person_of base in
  let couple = couple_of base in
  match w with
  | BigAgeBetweenSpouses (p1, p2, a) ->
      BigAgeBetweenSpouses (pers p1, pers p2, a.year)
  | BirthAfterDeath p -> BirthAfterDeath (pers p)
  | ChangedOrderOfChildren (ifam, _, _, _) ->
      ChangedOrderOfChildren (couple ifam)
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      ChangedOrderOfFamilyEvents (couple ifam)
  | ChangedOrderOfMarriages (p, _, _) -> ChangedOrderOfMarriages (pers p)
  | ChangedOrderOfPersonEvents (p, _, _) -> ChangedOrderOfPersonEvents (pers p)
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      ChildrenNotInOrder (couple ifam, pers elder, pers x)
  | CloseChildren (ifam, c1, c2) -> CloseChildren (couple ifam, pers c1, pers c2)
  | DeadOld (p, a) -> DeadOld (pers p, a.year)
  | DeadTooEarlyToBeFather (father, child) ->
      DeadTooEarlyToBeFather (pers father, pers child)
  | DistantChildren (ifam, c1, c2) ->
      DistantChildren (couple ifam, pers c1, pers c2)
  | FEventOrder (p, e1, e2) ->
      FEventOrder (pers p, fevent_of base e1, fevent_of base e2)
  | FWitnessEventAfterDeath (p, e, ifam) ->
      FWitnessEventAfterDeath (pers p, fevent_of base e, couple ifam)
  | FWitnessEventBeforeBirth (p, e, ifam) ->
      FWitnessEventBeforeBirth (pers p, fevent_of base e, couple ifam)
  | IncoherentAncestorDate (anc, p) -> IncoherentAncestorDate (pers anc, pers p)
  | IncoherentSex (p, _, _) -> IncoherentSex (pers p)
  | MarriageDateAfterDeath p -> MarriageDateAfterDeath (pers p)
  | MarriageDateBeforeBirth p -> MarriageDateBeforeBirth (pers p)
  | MotherDeadBeforeChildBirth (mother, child) ->
      MotherDeadBeforeChildBirth (pers mother, pers child)
  | OldForMarriage (p, a, ifam) -> OldForMarriage (pers p, a.year, couple ifam)
  | ParentBornAfterChild (parent, child) ->
      ParentBornAfterChild (pers parent, pers child)
  | ParentTooOld (p, a, child) -> ParentTooOld (pers p, a.year, pers child)
  | ParentTooYoung (p, a, child) -> ParentTooYoung (pers p, a.year, pers child)
  | PEventOrder (p, e1, e2) ->
      PEventOrder (pers p, pevent_of base e1, pevent_of base e2)
  | PossibleDuplicateFam (f1, _) -> PossibleDuplicateFam (couple f1)
  | PossibleDuplicateFamHomonymous (f1, _, p) ->
      PossibleDuplicateFamHomonymous (couple f1, pers p)
  | PWitnessEventAfterDeath (p, e, origin) ->
      PWitnessEventAfterDeath (pers p, pevent_of base e, pers origin)
  | PWitnessEventBeforeBirth (p, e, origin) ->
      PWitnessEventBeforeBirth (pers p, pevent_of base e, pers origin)
  | TitleDatesError (p, t) ->
      let title =
        String.trim (Driver.sou base t.t_ident ^ " " ^ Driver.sou base t.t_place)
      in
      TitleDatesError (pers p, title)
  | UndefinedSex p -> UndefinedSex (pers p)
  | YoungForMarriage (p, a, ifam) ->
      YoungForMarriage (pers p, a.year, couple ifam)

(* The same anomaly can be reported twice (a check run from both spouses, from
   both families of a duplicate pair): keep the first occurrence. *)
let dedup anomalies =
  let seen = Hashtbl.create 1024 in
  List.filter
    (fun a ->
      if Hashtbl.mem seen a then false
      else (
        Hashtbl.add seen a ();
        true))
    anomalies

let collector base =
  let acc = ref [] in
  ((fun w -> acc := of_warning base w :: !acc), fun () -> dedup (List.rev !acc))

let collect base =
  let add, result = collector base in
  Check.check_base base ignore add ignore;
  result ()

(* ---- report -------------------------------------------------------------- *)

type change = { time : string; wizard : string }
type signature = { ident : string; signed : string }

type who = {
  signers : signature list;
  first : change option;
  last : change option;
}

type item = { anomaly : t; who : who list }
type report = { date : string; producer : string; items : item list }

let nobody = { signers = []; first = None; last = None }
let format_version = 2

let stamp_of_time t =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

(* ---- signatures in the sources ------------------------------------------- *)

(* The "sign" button of the update forms appends "[[w:ident]] (source)
   day/ROMAN-MONTH/year" to the source fields. *)
let re_signature = Str.regexp {|\[\[w:\([^]|]+\)\(|[^]]*\)?\]\]|}

let re_sign_date =
  Str.regexp {|\([0-9][0-9]?\)/\([IVXivx]+\)/\([0-9][0-9][0-9][0-9]\)|}

let roman_month s =
  match String.uppercase_ascii s with
  | "I" -> Some 1
  | "II" -> Some 2
  | "III" -> Some 3
  | "IV" -> Some 4
  | "V" -> Some 5
  | "VI" -> Some 6
  | "VII" -> Some 7
  | "VIII" -> Some 8
  | "IX" -> Some 9
  | "X" -> Some 10
  | "XI" -> Some 11
  | "XII" -> Some 12
  | _ -> None

(* the signatures of a text, in order, each with the date that follows it
   (before the next signature), "YYYY-MM-DD" or "" *)
let signatures_in s =
  let rec loop pos acc =
    match Str.search_forward re_signature s pos with
    | exception Not_found -> List.rev acc
    | _ ->
        let ident = String.trim (Str.matched_group 1 s) in
        let after = Str.match_end () in
        let next =
          match Str.search_forward re_signature s after with
          | j -> j
          | exception Not_found -> String.length s
        in
        let signed =
          match Str.search_forward re_sign_date s after with
          | j when j < next -> (
              match roman_month (Str.matched_group 2 s) with
              | Some m ->
                  Printf.sprintf "%s-%02d-%02d" (Str.matched_group 3 s) m
                    (int_of_string (Str.matched_group 1 s))
              | None -> "")
          | _ -> ""
          | exception Not_found -> ""
        in
        loop after (if ident = "" then acc else { ident; signed } :: acc)
  in
  loop 0 []

(* Chronological when every signature is dated, else in text order; a
   signature repeated in several fields counts once. *)
let order_signatures sigs =
  let seen = Hashtbl.create 8 in
  let sigs =
    List.filter
      (fun g ->
        if Hashtbl.mem seen g then false
        else (
          Hashtbl.add seen g ();
          true))
      sigs
  in
  if List.for_all (fun g -> g.signed <> "") sigs then
    List.stable_sort (fun g1 g2 -> compare g1.signed g2.signed) sigs
  else sigs

let person_sources base p =
  let s = Driver.sou base in
  s (Driver.get_psources p)
  :: s (Driver.get_birth_src p)
  :: s (Driver.get_baptism_src p)
  :: s (Driver.get_death_src p)
  :: s (Driver.get_burial_src p)
  :: List.map
       (fun (e : Driver.pers_event) -> s e.epers_src)
       (Driver.get_pevents p)

let family_sources base fam =
  let s = Driver.sou base in
  s (Driver.get_fsources fam)
  :: s (Driver.get_marriage_src fam)
  :: List.map
       (fun (e : Driver.fam_event) -> s e.efam_src)
       (Driver.get_fevents fam)

let signatures_of_text = signatures_in

let signers_of sources =
  order_signatures (List.concat_map signatures_in sources)

(* ---- history ------------------------------------------------------------- *)

(* A history line is "YYYY-MM-DD HH:MM:SS [wizard] action (fn.occ sn)", written
   by History.record; times sort as strings. For the persons in [wanted], the
   first and the last line naming them. The file is read line by line. *)
let history_changes ~history_file wanted =
  let tbl = Hashtbl.create 1024 in
  let parse line =
    let len = String.length line in
    if len > 22 && line.[20] = '[' && line.[len - 1] = ')' then
      match String.index_from_opt line 21 ']' with
      | None -> ()
      | Some j -> (
          match String.rindex_opt line '(' with
          | Some k when k > j ->
              let key = String.sub line (k + 1) (len - k - 2) in
              if Hashtbl.mem wanted key then
                let c =
                  {
                    time = String.sub line 0 19;
                    wizard = String.sub line 21 (j - 21);
                  }
                in
                Hashtbl.replace tbl key
                  (match Hashtbl.find_opt tbl key with
                  | Some (first, last) ->
                      ( (if c.time < first.time then c else first),
                        if c.time >= last.time then c else last )
                  | None -> (c, c))
          | Some _ | None -> ())
  in
  (match open_in history_file with
  | exception Sys_error _ -> ()
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop () =
            match input_line ic with
            | line ->
                parse line;
                loop ()
            | exception End_of_file -> ()
          in
          loop ()));
  tbl

let earliest a b =
  match (a, b) with
  | Some x, Some y -> Some (if y.time < x.time then y else x)
  | Some _, None -> a
  | None, _ -> b

let latest a b =
  match (a, b) with
  | Some x, Some y -> Some (if y.time >= x.time then y else x)
  | Some _, None -> a
  | None, _ -> b

let make ~base ~bdir ~producer anomalies =
  let wanted = Hashtbl.create 4096 in
  List.iter
    (fun a ->
      List.iter
        (fun s ->
          match s with
          | Person p -> Hashtbl.replace wanted (person_key p) ()
          | Couple c ->
              Hashtbl.replace wanted (person_key c.father) ();
              Hashtbl.replace wanted (person_key c.mother) ())
        (subjects a))
    anomalies;
  let hist =
    history_changes ~history_file:(Filename.concat bdir "history") wanted
  in
  let history p =
    match Hashtbl.find_opt hist (person_key p) with
    | Some (first, last) -> (Some first, Some last)
    | None -> (None, None)
  in
  let resolve p =
    if is_unnamed p then None
    else
      Option.map (Driver.poi base)
        (Driver.person_of_key base p.first_name p.surname p.occ)
  in
  let cache = Hashtbl.create 4096 in
  let who_of_subject s =
    match Hashtbl.find_opt cache s with
    | Some w -> w
    | None ->
        let w =
          match s with
          | Person p ->
              let first, last = history p in
              let signers =
                match resolve p with
                | Some dp -> signers_of (person_sources base dp)
                | None -> []
              in
              { signers; first; last }
          | Couple c ->
              let f1, l1 = history c.father and f2, l2 = history c.mother in
              let signers =
                match (resolve c.father, resolve c.mother) with
                | Some f, Some m ->
                    let ip_m = Driver.get_iper m in
                    Driver.get_family f |> Array.to_list
                    |> List.filter_map (fun ifam ->
                        let fam = Driver.foi base ifam in
                        if
                          Driver.Iper.equal (Driver.get_mother fam) ip_m
                          || Driver.Iper.equal (Driver.get_father fam) ip_m
                        then Some fam
                        else None)
                    |> List.concat_map (family_sources base)
                    |> signers_of
                    |> fun family_signers ->
                    (* a union often carries no source of its own: then the
                       spouses' signatures *)
                    if family_signers <> [] then family_signers
                    else
                      signers_of
                        (List.concat_map (person_sources base) [ f; m ])
                | Some _, None | None, Some _ | None, None -> []
              in
              { signers; first = earliest f1 f2; last = latest l1 l2 }
        in
        Hashtbl.add cache s w;
        w
  in
  {
    date = stamp_of_time (Unix.time ());
    producer;
    items =
      List.map
        (fun a -> { anomaly = a; who = List.map who_of_subject (subjects a) })
        anomalies;
  }

(* ---- responsible wizard --------------------------------------------------- *)

type rule = First_signer | Last_signer

let rule_of_string s =
  match s with "first_signer" -> First_signer | _ -> Last_signer

type responsible = { by : string; signed : bool; at : string }

let responsible rule item =
  let last l = List.fold_left (fun _ x -> Some x) None l in
  match List.concat_map (fun w -> w.signers) item.who with
  | _ :: _ as sigs -> (
      let sigs = order_signatures sigs in
      let pick =
        match (rule, sigs) with
        | First_signer, g :: _ -> Some g
        | First_signer, [] -> None
        | Last_signer, _ -> last sigs
      in
      match pick with
      | Some g -> Some { by = g.ident; signed = true; at = g.signed }
      | None -> None)
  | [] -> (
      let change =
        List.fold_left
          (fun acc w ->
            match rule with
            | First_signer -> earliest acc w.first
            | Last_signer -> latest acc w.last)
          None item.who
      in
      match change with
      | Some c ->
          Some { by = c.wizard; signed = false; at = String.sub c.time 0 10 }
      | None -> None)

(* ---- JSON ---------------------------------------------------------------- *)

(* Each anomaly is {"k": kind name, "p": persons, "c": couples, "n": integers,
   "e": events, "s": strings}, the fields of its constructor in order; empty
   fields are omitted. A person is [first_name, occ, surname]. A built-in
   event is its constructor name; a custom one is ["name"]. *)

let pevent_names =
  [
    ("Birth", Epers_Birth);
    ("Baptism", Epers_Baptism);
    ("Death", Epers_Death);
    ("Burial", Epers_Burial);
    ("Cremation", Epers_Cremation);
    ("Accomplishment", Epers_Accomplishment);
    ("Acquisition", Epers_Acquisition);
    ("Adhesion", Epers_Adhesion);
    ("BaptismLDS", Epers_BaptismLDS);
    ("BarMitzvah", Epers_BarMitzvah);
    ("BatMitzvah", Epers_BatMitzvah);
    ("Benediction", Epers_Benediction);
    ("ChangeName", Epers_ChangeName);
    ("Circumcision", Epers_Circumcision);
    ("Confirmation", Epers_Confirmation);
    ("ConfirmationLDS", Epers_ConfirmationLDS);
    ("Decoration", Epers_Decoration);
    ("DemobilisationMilitaire", Epers_DemobilisationMilitaire);
    ("Diploma", Epers_Diploma);
    ("Distinction", Epers_Distinction);
    ("Dotation", Epers_Dotation);
    ("DotationLDS", Epers_DotationLDS);
    ("Education", Epers_Education);
    ("Election", Epers_Election);
    ("Emigration", Epers_Emigration);
    ("Excommunication", Epers_Excommunication);
    ("FamilyLinkLDS", Epers_FamilyLinkLDS);
    ("FirstCommunion", Epers_FirstCommunion);
    ("Funeral", Epers_Funeral);
    ("Graduate", Epers_Graduate);
    ("Hospitalisation", Epers_Hospitalisation);
    ("Illness", Epers_Illness);
    ("Immigration", Epers_Immigration);
    ("ListePassenger", Epers_ListePassenger);
    ("MilitaryDistinction", Epers_MilitaryDistinction);
    ("MilitaryPromotion", Epers_MilitaryPromotion);
    ("MilitaryService", Epers_MilitaryService);
    ("MobilisationMilitaire", Epers_MobilisationMilitaire);
    ("Naturalisation", Epers_Naturalisation);
    ("Occupation", Epers_Occupation);
    ("Ordination", Epers_Ordination);
    ("Property", Epers_Property);
    ("Recensement", Epers_Recensement);
    ("Residence", Epers_Residence);
    ("Retired", Epers_Retired);
    ("ScellentChildLDS", Epers_ScellentChildLDS);
    ("ScellentParentLDS", Epers_ScellentParentLDS);
    ("ScellentSpouseLDS", Epers_ScellentSpouseLDS);
    ("VenteBien", Epers_VenteBien);
    ("Will", Epers_Will);
  ]

let fevent_names =
  [
    ("Marriage", Efam_Marriage);
    ("NoMarriage", Efam_NoMarriage);
    ("NoMention", Efam_NoMention);
    ("Engage", Efam_Engage);
    ("Divorce", Efam_Divorce);
    ("Separated", Efam_Separated);
    ("Annulation", Efam_Annulation);
    ("MarriageBann", Efam_MarriageBann);
    ("MarriageContract", Efam_MarriageContract);
    ("MarriageLicense", Efam_MarriageLicense);
    ("PACS", Efam_PACS);
    ("Residence", Efam_Residence);
  ]

let event_to_json names e =
  match List.find_opt (fun (_, c) -> c = e) names with
  | Some (n, _) -> `String n
  | None -> `Null

let pevent_to_json (e : pevent) =
  match e with
  | Epers_Name s -> `List [ `String s ]
  | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial | Epers_Cremation
  | Epers_Accomplishment | Epers_Acquisition | Epers_Adhesion | Epers_BaptismLDS
  | Epers_BarMitzvah | Epers_BatMitzvah | Epers_Benediction | Epers_ChangeName
  | Epers_Circumcision | Epers_Confirmation | Epers_ConfirmationLDS
  | Epers_Decoration | Epers_DemobilisationMilitaire | Epers_Diploma
  | Epers_Distinction | Epers_Dotation | Epers_DotationLDS | Epers_Education
  | Epers_Election | Epers_Emigration | Epers_Excommunication
  | Epers_FamilyLinkLDS | Epers_FirstCommunion | Epers_Funeral | Epers_Graduate
  | Epers_Hospitalisation | Epers_Illness | Epers_Immigration
  | Epers_ListePassenger | Epers_MilitaryDistinction | Epers_MilitaryPromotion
  | Epers_MilitaryService | Epers_MobilisationMilitaire | Epers_Naturalisation
  | Epers_Occupation | Epers_Ordination | Epers_Property | Epers_Recensement
  | Epers_Residence | Epers_Retired | Epers_ScellentChildLDS
  | Epers_ScellentParentLDS | Epers_ScellentSpouseLDS | Epers_VenteBien
  | Epers_Will ->
      event_to_json pevent_names e

let fevent_to_json (e : fevent) =
  match e with
  | Efam_Name s -> `List [ `String s ]
  | Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
  | Efam_Divorce | Efam_Separated | Efam_Annulation | Efam_MarriageBann
  | Efam_MarriageContract | Efam_MarriageLicense | Efam_PACS | Efam_Residence ->
      event_to_json fevent_names e

let event_of_json names custom (j : Yojson.Safe.t) =
  match j with
  | `String n -> List.assoc_opt n names
  | `List [ `String s ] -> Some (custom s)
  | _ -> None

let pevent_of_json = event_of_json pevent_names (fun s -> Epers_Name s)
let fevent_of_json = event_of_json fevent_names (fun s -> Efam_Name s)

let person_to_json p =
  `List [ `String p.first_name; `Int p.occ; `String p.surname ]

let person_of_json (j : Yojson.Safe.t) =
  match j with
  | `List [ `String first_name; `Int occ; `String surname ] ->
      Some { first_name; surname; occ }
  | _ -> None

let couple_to_json c =
  `List [ person_to_json c.father; person_to_json c.mother ]

let couple_of_json (j : Yojson.Safe.t) =
  match j with
  | `List [ f; m ] -> (
      match (person_of_json f, person_of_json m) with
      | Some father, Some mother -> Some { father; mother }
      | None, _ | _, None -> None)
  | _ -> None

type fields = {
  p : person list;
  c : couple list;
  n : int list;
  e : Yojson.Safe.t list;
  s : string list;
}

let no_fields = { p = []; c = []; n = []; e = []; s = [] }

let fields_of a =
  let f = no_fields in
  match a with
  | BigAgeBetweenSpouses (p1, p2, n) -> { f with p = [ p1; p2 ]; n = [ n ] }
  | BirthAfterDeath p
  | ChangedOrderOfMarriages p
  | ChangedOrderOfPersonEvents p
  | IncoherentSex p
  | MarriageDateAfterDeath p
  | MarriageDateBeforeBirth p
  | UndefinedSex p ->
      { f with p = [ p ] }
  | ChangedOrderOfChildren c
  | ChangedOrderOfFamilyEvents c
  | PossibleDuplicateFam c ->
      { f with c = [ c ] }
  | ChildrenNotInOrder (c, p1, p2)
  | CloseChildren (c, p1, p2)
  | DistantChildren (c, p1, p2) ->
      { f with c = [ c ]; p = [ p1; p2 ] }
  | DeadOld (p, n) -> { f with p = [ p ]; n = [ n ] }
  | DeadTooEarlyToBeFather (p1, p2)
  | IncoherentAncestorDate (p1, p2)
  | MotherDeadBeforeChildBirth (p1, p2)
  | ParentBornAfterChild (p1, p2) ->
      { f with p = [ p1; p2 ] }
  | FEventOrder (p, e1, e2) ->
      { f with p = [ p ]; e = [ fevent_to_json e1; fevent_to_json e2 ] }
  | FWitnessEventAfterDeath (p, e, c) | FWitnessEventBeforeBirth (p, e, c) ->
      { f with p = [ p ]; e = [ fevent_to_json e ]; c = [ c ] }
  | OldForMarriage (p, n, c) | YoungForMarriage (p, n, c) ->
      { f with p = [ p ]; n = [ n ]; c = [ c ] }
  | ParentTooOld (p1, n, p2) | ParentTooYoung (p1, n, p2) ->
      { f with p = [ p1; p2 ]; n = [ n ] }
  | PEventOrder (p, e1, e2) ->
      { f with p = [ p ]; e = [ pevent_to_json e1; pevent_to_json e2 ] }
  | PossibleDuplicateFamHomonymous (c, p) -> { f with c = [ c ]; p = [ p ] }
  | PWitnessEventAfterDeath (p1, e, p2) | PWitnessEventBeforeBirth (p1, e, p2)
    ->
      { f with p = [ p1; p2 ]; e = [ pevent_to_json e ] }
  | TitleDatesError (p, s) -> { f with p = [ p ]; s = [ s ] }

let of_fields k f =
  let pe = pevent_of_json and fe = fevent_of_json in
  match (k, f.p, f.c, f.n, f.e, f.s) with
  | K_BigAgeBetweenSpouses, [ p1; p2 ], [], [ n ], [], [] ->
      Some (BigAgeBetweenSpouses (p1, p2, n))
  | K_BirthAfterDeath, [ p ], [], [], [], [] -> Some (BirthAfterDeath p)
  | K_ChangedOrderOfMarriages, [ p ], [], [], [], [] ->
      Some (ChangedOrderOfMarriages p)
  | K_ChangedOrderOfPersonEvents, [ p ], [], [], [], [] ->
      Some (ChangedOrderOfPersonEvents p)
  | K_IncoherentSex, [ p ], [], [], [], [] -> Some (IncoherentSex p)
  | K_MarriageDateAfterDeath, [ p ], [], [], [], [] ->
      Some (MarriageDateAfterDeath p)
  | K_MarriageDateBeforeBirth, [ p ], [], [], [], [] ->
      Some (MarriageDateBeforeBirth p)
  | K_UndefinedSex, [ p ], [], [], [], [] -> Some (UndefinedSex p)
  | K_ChangedOrderOfChildren, [], [ c ], [], [], [] ->
      Some (ChangedOrderOfChildren c)
  | K_ChangedOrderOfFamilyEvents, [], [ c ], [], [], [] ->
      Some (ChangedOrderOfFamilyEvents c)
  | K_PossibleDuplicateFam, [], [ c ], [], [], [] ->
      Some (PossibleDuplicateFam c)
  | K_ChildrenNotInOrder, [ p1; p2 ], [ c ], [], [], [] ->
      Some (ChildrenNotInOrder (c, p1, p2))
  | K_CloseChildren, [ p1; p2 ], [ c ], [], [], [] ->
      Some (CloseChildren (c, p1, p2))
  | K_DistantChildren, [ p1; p2 ], [ c ], [], [], [] ->
      Some (DistantChildren (c, p1, p2))
  | K_DeadOld, [ p ], [], [ n ], [], [] -> Some (DeadOld (p, n))
  | K_DeadTooEarlyToBeFather, [ p1; p2 ], [], [], [], [] ->
      Some (DeadTooEarlyToBeFather (p1, p2))
  | K_IncoherentAncestorDate, [ p1; p2 ], [], [], [], [] ->
      Some (IncoherentAncestorDate (p1, p2))
  | K_MotherDeadBeforeChildBirth, [ p1; p2 ], [], [], [], [] ->
      Some (MotherDeadBeforeChildBirth (p1, p2))
  | K_ParentBornAfterChild, [ p1; p2 ], [], [], [], [] ->
      Some (ParentBornAfterChild (p1, p2))
  | K_FEventOrder, [ p ], [], [], [ e1; e2 ], [] -> (
      match (fe e1, fe e2) with
      | Some e1, Some e2 -> Some (FEventOrder (p, e1, e2))
      | None, _ | _, None -> None)
  | K_FWitnessEventAfterDeath, [ p ], [ c ], [], [ e ], [] ->
      Option.map (fun e -> FWitnessEventAfterDeath (p, e, c)) (fe e)
  | K_FWitnessEventBeforeBirth, [ p ], [ c ], [], [ e ], [] ->
      Option.map (fun e -> FWitnessEventBeforeBirth (p, e, c)) (fe e)
  | K_OldForMarriage, [ p ], [ c ], [ n ], [], [] ->
      Some (OldForMarriage (p, n, c))
  | K_YoungForMarriage, [ p ], [ c ], [ n ], [], [] ->
      Some (YoungForMarriage (p, n, c))
  | K_ParentTooOld, [ p1; p2 ], [], [ n ], [], [] ->
      Some (ParentTooOld (p1, n, p2))
  | K_ParentTooYoung, [ p1; p2 ], [], [ n ], [], [] ->
      Some (ParentTooYoung (p1, n, p2))
  | K_PEventOrder, [ p ], [], [], [ e1; e2 ], [] -> (
      match (pe e1, pe e2) with
      | Some e1, Some e2 -> Some (PEventOrder (p, e1, e2))
      | None, _ | _, None -> None)
  | K_PossibleDuplicateFamHomonymous, [ p ], [ c ], [], [], [] ->
      Some (PossibleDuplicateFamHomonymous (c, p))
  | K_PWitnessEventAfterDeath, [ p1; p2 ], [], [], [ e ], [] ->
      Option.map (fun e -> PWitnessEventAfterDeath (p1, e, p2)) (pe e)
  | K_PWitnessEventBeforeBirth, [ p1; p2 ], [], [], [ e ], [] ->
      Option.map (fun e -> PWitnessEventBeforeBirth (p1, e, p2)) (pe e)
  | K_TitleDatesError, [ p ], [], [], [], [ s ] -> Some (TitleDatesError (p, s))
  | ( ( K_BigAgeBetweenSpouses | K_BirthAfterDeath | K_ChangedOrderOfChildren
      | K_ChangedOrderOfFamilyEvents | K_ChangedOrderOfMarriages
      | K_ChangedOrderOfPersonEvents | K_ChildrenNotInOrder | K_CloseChildren
      | K_DeadOld | K_DeadTooEarlyToBeFather | K_DistantChildren | K_FEventOrder
      | K_FWitnessEventAfterDeath | K_FWitnessEventBeforeBirth
      | K_IncoherentAncestorDate | K_IncoherentSex | K_MarriageDateAfterDeath
      | K_MarriageDateBeforeBirth | K_MotherDeadBeforeChildBirth
      | K_OldForMarriage | K_ParentBornAfterChild | K_ParentTooOld
      | K_ParentTooYoung | K_PEventOrder | K_PossibleDuplicateFam
      | K_PossibleDuplicateFamHomonymous | K_PWitnessEventAfterDeath
      | K_PWitnessEventBeforeBirth | K_TitleDatesError | K_UndefinedSex
      | K_YoungForMarriage ),
      _,
      _,
      _,
      _,
      _ ) ->
      None

let to_json a =
  let f = fields_of a in
  let field (name, l) = if l = [] then None else Some (name, `List l) in
  `Assoc
    (("k", `String (kind_name (kind a)))
    :: List.filter_map field
         [
           ("p", List.map person_to_json f.p);
           ("c", List.map couple_to_json f.c);
           ("n", List.map (fun n -> `Int n) f.n);
           ("e", f.e);
           ("s", List.map (fun s -> `String s) f.s);
         ])

(* [Some l] when every element converts, [None] otherwise *)
let all_some conv l =
  List.fold_right
    (fun x acc ->
      match (conv x, acc) with
      | Some y, Some acc -> Some (y :: acc)
      | None, _ | _, None -> None)
    l (Some [])

let of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc fields -> (
      let list name =
        match List.assoc_opt name fields with Some (`List l) -> l | _ -> []
      in
      let int_of (j : Yojson.Safe.t) =
        match j with `Int n -> Some n | _ -> None
      in
      let string_of (j : Yojson.Safe.t) =
        match j with `String s -> Some s | _ -> None
      in
      match
        ( Option.bind (List.assoc_opt "k" fields) string_of
          |> Fun.flip Option.bind kind_of_name,
          all_some person_of_json (list "p"),
          all_some couple_of_json (list "c"),
          all_some int_of (list "n"),
          all_some string_of (list "s") )
      with
      | Some k, Some p, Some c, Some n, Some s ->
          of_fields k { p; c; n; e = list "e"; s }
      | None, _, _, _, _
      | _, None, _, _, _
      | _, _, None, _, _
      | _, _, _, None, _
      | _, _, _, _, None ->
          None)
  | _ -> None

(* Fields separated by a control character, which names never contain. *)
let identity a =
  let f = fields_of a in
  let b = Buffer.create 80 in
  let add s =
    Buffer.add_char b '\x1f';
    Buffer.add_string b s
  in
  Buffer.add_string b (kind_name (kind a));
  List.iter (fun p -> add (person_key p)) f.p;
  List.iter
    (fun c ->
      add (person_key c.father);
      add (person_key c.mother))
    f.c;
  List.iter (fun e -> add (Yojson.Safe.to_string e)) f.e;
  List.iter add f.s;
  Buffer.contents b

(* The report is JSON Lines: a header object on the first line, then one
   anomaly per line. gwc writes it without building one large JSON tree, and
   gwd reads it line by line, skipping the lines of other kinds unparsed. *)

let change_to_json c = `List [ `String c.time; `String c.wizard ]

let change_of_json (j : Yojson.Safe.t) =
  match j with
  | `List [ `String time; `String wizard ] -> Some { time; wizard }
  | _ -> None

let who_to_json w =
  `Assoc
    (List.filter_map Fun.id
       [
         (if w.signers = [] then None
          else
            Some
              ( "s",
                `List
                  (List.map
                     (fun g -> `List [ `String g.ident; `String g.signed ])
                     w.signers) ));
         Option.map (fun c -> ("f", change_to_json c)) w.first;
         Option.map (fun c -> ("l", change_to_json c)) w.last;
       ])

let who_of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc fields ->
      let signers =
        match List.assoc_opt "s" fields with
        | Some (`List l) ->
            List.filter_map
              (fun (g : Yojson.Safe.t) ->
                match g with
                | `List [ `String ident; `String signed ] ->
                    Some { ident; signed }
                | _ -> None)
              l
        | _ -> []
      in
      {
        signers;
        first = Option.bind (List.assoc_opt "f" fields) change_of_json;
        last = Option.bind (List.assoc_opt "l" fields) change_of_json;
      }
  | _ -> nobody

(* an item is its anomaly with a "w" field: one entry per subject *)
let item_to_json it =
  match to_json it.anomaly with
  | `Assoc fields ->
      `Assoc
        (List.rev
           (("w", `List (List.map who_to_json it.who)) :: List.rev fields))
  | j -> j

let item_of_json (j : Yojson.Safe.t) =
  match (of_json j, j) with
  | Some anomaly, `Assoc fields ->
      let who =
        match List.assoc_opt "w" fields with
        | Some (`List l) -> List.map who_of_json l
        | _ -> []
      in
      Some { anomaly; who }
  | Some _, _ | None, _ -> None

(* The report is JSON Lines: a header object on the first line, then one
   anomaly per line. gwc writes it without building one large JSON tree, and
   gwd reads it line by line, skipping the lines of other kinds unparsed. *)

let header_to_json r =
  `Assoc
    [
      ("version", `Int format_version);
      ("date", `String r.date);
      ("producer", `String r.producer);
      ("count", `Int (List.length r.items));
    ]

let header_of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc fields -> (
      let str name =
        match List.assoc_opt name fields with Some (`String s) -> s | _ -> ""
      in
      match List.assoc_opt "version" fields with
      | Some (`Int v) when v = format_version ->
          Ok { date = str "date"; producer = str "producer"; items = [] }
      | Some _ | None ->
          Error "unknown anomalies report format version: run anoma again")
  | _ -> Error "the anomalies report does not start with a JSON object"

let file config_dir = Filename.concat config_dir "anomalies.jsonl"
let exists config_dir = Sys.file_exists (file config_dir)

let save config_dir r =
  Filesystem.create_dir ~parent:true config_dir;
  let path = file config_dir in
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      let line j =
        Yojson.Safe.to_channel oc j;
        output_char oc '\n'
      in
      line (header_to_json r);
      List.iter (fun it -> line (item_to_json it)) r.items);
  Sys.rename tmp path

(* to_json puts "k" first: a line starts with {"k":"<kind name>", the
   closing quote keeping PossibleDuplicateFam apart from
   PossibleDuplicateFamHomonymous *)
let kind_prefix k = Printf.sprintf {|{"k":"%s"|} (kind_name k)

let fold ?kind config_dir f init =
  match open_in_bin (file config_dir) with
  | exception Sys_error msg -> Error msg
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match Yojson.Safe.from_string (input_line ic) with
          | exception End_of_file -> Error "empty anomalies report"
          | exception Yojson.Json_error msg -> Error msg
          | j -> (
              match header_of_json j with
              | Error _ as e -> e
              | Ok header ->
                  let wanted =
                    match kind with
                    | None -> fun _ -> true
                    | Some k ->
                        let prefix = kind_prefix k in
                        fun line -> String.starts_with ~prefix line
                  in
                  let rec loop acc =
                    match input_line ic with
                    | exception End_of_file -> acc
                    | line when not (wanted line) -> loop acc
                    | line -> (
                        match Yojson.Safe.from_string line with
                        | exception Yojson.Json_error _ -> loop acc
                        | j -> (
                            match item_of_json j with
                            | Some it -> loop (f acc it)
                            | None -> loop acc))
                  in
                  Ok (header, loop init)))

let load config_dir =
  Result.map
    (fun (header, rev) -> { header with items = List.rev rev })
    (fold config_dir (fun acc it -> it :: acc) [])

let write ~base ~bdir ~producer anomalies =
  let dir = Filename.concat bdir "config" in
  match save dir (make ~base ~bdir ~producer anomalies) with
  | () -> ()
  | exception (Sys_error msg | Failure msg) ->
      Printf.eprintf "Cannot write the anomalies report: %s\n%!" msg
  | exception Unix.Unix_error (e, _, arg) ->
      Printf.eprintf "Cannot write the anomalies report: %s %s\n%!"
        (Unix.error_message e) arg
