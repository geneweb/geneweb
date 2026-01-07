(*TODO why polymorphic *)

(** Database warnings attached to the specification of the person, family,
    relation, etc. *)
type ('iper, 'person, 'family, 'descend, 'title, 'pevent, 'fevent) warning =
  | BigAgeBetweenSpouses of
      'person
      * 'person
      * Date.dmy (* Age differece between couples is greater then 50 years *)
  | BirthAfterDeath of 'person  (** Person is born after his death *)
  | IncoherentSex of 'person * int * int  (** Incoherent sex of person *)
  | ChangedOrderOfChildren of 'family * 'descend * 'iper array * 'iper array
      (** Children order has been modified *)
  | ChangedOrderOfMarriages of 'person * 'family array * 'family array
      (** Person's marriages order has been modified *)
  | ChangedOrderOfFamilyEvents of 'family * 'fevent list * 'fevent list
      (** Family's events order has been modified *)
  | ChangedOrderOfPersonEvents of 'person * 'pevent list * 'pevent list
      (** Person's events order has been modified *)
  | ChildrenNotInOrder of 'family * 'descend * 'person * 'person
      (** Children aren't ordered *)
  | CloseChildren of 'family * 'person * 'person
      (** Age difference between two child is less then 7 month (except for
          twins) *)
  | DeadOld of 'person * Date.dmy
      (** Dead old (at the age older then 109 after 1900 year and older then 100
          before) *)
  | DeadTooEarlyToBeFather of 'person * 'person
      (** Children is born in more then 1 year after his father's death *)
  | DistantChildren of 'family * 'person * 'person
      (** Age gap between two of siblings greater then 50 years *)
  | FEventOrder of 'person * 'fevent * 'fevent
      (** Familial events haven't been ordered correctly *)
  | FWitnessEventAfterDeath of 'person * 'fevent * 'family
      (** Witness is dead before familial event date *)
  | FWitnessEventBeforeBirth of 'person * 'fevent * 'family
      (** Witness is born after familial event date *)
  | IncoherentAncestorDate of 'person * 'person
      (** Ancestor is born after person's birth *)
  | MarriageDateAfterDeath of 'person  (** Person is married after his death *)
  | MarriageDateBeforeBirth of 'person
      (** Person is married before his birth *)
  | MotherDeadBeforeChildBirth of 'person * 'person
      (** Children is born after his mother's death *)
  | ParentBornAfterChild of 'person * 'person
      (** Parent is born after one of his children *)
  | ParentTooOld of 'person * Date.dmy * 'person
      (** Person became a parent at age older then 55 years for mother and 70
          for father *)
  | ParentTooYoung of 'person * Date.dmy * 'person
      (** Person became a parent at age younger then 11 years old *)
  | PEventOrder of 'person * 'pevent * 'pevent
      (** Personal events haven't been ordered correctly *)
  | PossibleDuplicateFam of 'family * 'family
      (** There is a possibility that two families are a duplicate of each other
      *)
  | PossibleDuplicateFamHomonymous of 'family * 'family * 'person
      (** There is a possibility that two families are a duplicate of each other
          (Homonymous spouse) *)
  | PWitnessEventAfterDeath of 'person * 'pevent * 'person
      (** Witness is dead before personal event date *)
  | PWitnessEventBeforeBirth of 'person * 'pevent * 'person
      (** Witness is born after personal event date *)
  | TitleDatesError of 'person * 'title
      (** Title's start date is after end date or person is born after title
          dates *)
  | UndefinedSex of 'person  (** Person has undefined sex (Neuter) *)
  | YoungForMarriage of 'person * Date.dmy * 'family
      (** Person is married before he was 12 years old *)
  | OldForMarriage of 'person * Date.dmy * 'family
      (** Person is married after he was 100 years old *)

(** Missing sources warning *)
type ('person, 'descend, 'title) misc = MissingSources

type base_warning =
  ( Gwdb.iper,
    Gwdb.person,
    Gwdb.ifam,
    Gwdb.family,
    Gwdb.title,
    (Gwdb.iper, Gwdb.istr) Def.gen_pers_event,
    (Gwdb.iper, Gwdb.istr) Def.gen_fam_event )
  warning
(** Database specification warning *)

type base_misc = (Gwdb.person, Gwdb.family, Gwdb.title) misc

let int_of_warning_tag = function
  | PossibleDuplicateFam (_f1, _f2) -> 0
  | PossibleDuplicateFamHomonymous (_f1, _f2, _p) -> 1
  | BigAgeBetweenSpouses (_p1, _p2, _d) -> 2
  | BirthAfterDeath _ -> 3
  | IncoherentSex (_p, _s1, _s2) -> 4
  | ChangedOrderOfChildren (_ifam, _fam, _ipers1, _ipers2) -> 5
  | ChangedOrderOfMarriages (_p, _ifams, _ifams2) -> 6
  | ChangedOrderOfFamilyEvents (_ifam, _fevents, _fevents2) -> 7
  | ChangedOrderOfPersonEvents (_p, _pevents, _pevents2) -> 8
  | ChildrenNotInOrder (_ifam, _fam, _p1, _p2) -> 9
  | CloseChildren (_ifam, _p1, _p2) -> 10
  | DeadOld (_p, _d) -> 11
  | DeadTooEarlyToBeFather (_p1, _p2) -> 12
  | DistantChildren (_ifam, _p1, _p2) -> 13
  | FEventOrder (_p, _fevent, _fevent2) -> 14
  | FWitnessEventAfterDeath (_p, _fevent, _ifam) -> 15
  | FWitnessEventBeforeBirth (_p, _fevent, _ifam) -> 16
  | IncoherentAncestorDate (_p1, _p2) -> 17
  | MarriageDateAfterDeath _p -> 18
  | MarriageDateBeforeBirth _p -> 19
  | MotherDeadBeforeChildBirth (_p1, _p2) -> 20
  | ParentBornAfterChild (_p1, _p2) -> 21
  | ParentTooOld (_p1, _d, _p2) -> 22
  | ParentTooYoung (_p1, _d, _p2) -> 23
  | PEventOrder (_p, _pevent1, _pevent2) -> 24
  | PWitnessEventAfterDeath (_p1, _pevent, _p2) -> 25
  | PWitnessEventBeforeBirth (_p1, _pevent, _p2) -> 26
  | TitleDatesError (_p, _title) -> 27
  | UndefinedSex _p -> 28
  | YoungForMarriage (_p, _d, _ifam) -> 29
  | OldForMarriage (_p, _d, _ifam) -> 30

let compare_family f1 f2 =
  Gwdb.compare_ifam (Gwdb.get_ifam f1) (Gwdb.get_ifam f2)

let compare_person p1 p2 =
  Gwdb.compare_iper (Gwdb.get_iper p1) (Gwdb.get_iper p2)

let normalize_warning (warning : base_warning) : base_warning =
  match warning with
  | PossibleDuplicateFam (f1, f2) ->
      if Gwdb.compare_ifam f2 f1 < 0 then PossibleDuplicateFam (f2, f1)
      else warning
  | PossibleDuplicateFamHomonymous (f1, f2, p) ->
      if Gwdb.compare_ifam f2 f1 < 0 then
        PossibleDuplicateFamHomonymous (f2, f1, p)
      else warning
  | BigAgeBetweenSpouses (p1, p2, d) ->
      if Gwdb.compare_iper (Gwdb.get_iper p2) (Gwdb.get_iper p1) < 0 then
        BigAgeBetweenSpouses (p2, p1, d)
      else warning
  | BirthAfterDeath _ -> warning
  | IncoherentSex (p, s1, s2) ->
      if s2 < s1 then IncoherentSex (p, s2, s1) else warning
  | ChangedOrderOfChildren (_ifam, _fam, _ipers1, _ipers2) -> warning
  | ChangedOrderOfMarriages (_p, _ifams, _ifams2) -> warning
  | ChangedOrderOfFamilyEvents (_ifam, _fevents, _fevents2) -> warning
  | ChangedOrderOfPersonEvents (_p, _pevents, _pevents2) -> warning
  | ChildrenNotInOrder (_ifam, _fam, _p1, _p2) -> warning
  | CloseChildren (ifam, p1, p2) ->
      if Gwdb.compare_iper (Gwdb.get_iper p2) (Gwdb.get_iper p1) < 0 then
        CloseChildren (ifam, p2, p1)
      else warning
  | DeadOld (_p, _d) -> warning
  | DeadTooEarlyToBeFather (_p1, _p2) -> warning
  | DistantChildren (ifam, p1, p2) ->
      if compare_person p2 p1 < 0 then DistantChildren (ifam, p2, p1)
      else warning
  | FEventOrder (_p, _fevent, _fevent2) -> warning
  | FWitnessEventAfterDeath (_p, _fevent, _ifam) -> warning
  | FWitnessEventBeforeBirth (_p, _fevent, _ifam) -> warning
  | IncoherentAncestorDate (_p1, _p2) -> warning
  | MarriageDateAfterDeath _p -> warning
  | MarriageDateBeforeBirth _p -> warning
  | MotherDeadBeforeChildBirth (_p1, _p2) -> warning
  | ParentBornAfterChild (_p1, _p2) -> warning
  | ParentTooOld (_p1, _d, _p2) -> warning
  | ParentTooYoung (_p1, _d, _p2) -> warning
  | PEventOrder (_p, _pevent1, _pevent2) -> warning
  | PWitnessEventAfterDeath (_p1, _pevent, _p2) -> warning
  | PWitnessEventBeforeBirth (_p1, _pevent, _p2) -> warning
  | TitleDatesError (_p, _title) -> warning
  | UndefinedSex _p -> warning
  | YoungForMarriage (_p, _d, _ifam) -> warning
  | OldForMarriage (_p, _d, _ifam) -> warning

let compare_array cmp arr1 arr2 =
  let module M = struct
    exception Stop of int
  end in
  let len1 = Array.length arr1 in
  let len2 = Array.length arr2 in
  if len1 = len2 then
    try
      Array.iter2
        (fun v1 v2 ->
          let c = cmp v1 v2 in
          if c <> 0 then raise (M.Stop c))
        arr1 arr2;
      0
    with M.Stop c -> c
  else len1 - len2

type name_info = { first_name : string; surname : string }
type couple_name_info = { father : name_info; mother : name_info }

let name_info_of_person base p =
  let first_name base p =
    Name.strip_lower (Gwdb.sou base (Gwdb.get_first_name p))
  in
  let surname base p = Name.strip_lower (Gwdb.sou base (Gwdb.get_surname p)) in
  { first_name = first_name base p; surname = surname base p }

let couple_name_info_of_family base fam =
  {
    father = name_info_of_person base (Gwdb.poi base (Gwdb.get_father fam));
    mother = name_info_of_person base (Gwdb.poi base (Gwdb.get_mother fam));
  }

module CoupleSet = Set.Make (struct
  type t = couple_name_info

  let compare = compare
end)

let rec handle_homonymous base cpl_set result ws =
  match ws with
  | (PossibleDuplicateFamHomonymous (ifam1, ifam2, _) as w) :: ws ->
      let fam1 = Gwdb.foi base ifam1 in
      let couple_name_info1 = couple_name_info_of_family base fam1 in
      let cpl_set' = CoupleSet.add couple_name_info1 cpl_set in
      let fam2 = Gwdb.foi base ifam2 in
      let couple_name_info2 = couple_name_info_of_family base fam2 in
      let cpl_set' = CoupleSet.add couple_name_info2 cpl_set' in
      let result = if cpl_set' == cpl_set then result else w :: result in
      handle_homonymous base cpl_set' result ws
  | w :: ws -> handle_homonymous base cpl_set (w :: result) ws
  | [] -> result

let handle_homonymous base warnings =
  handle_homonymous base CoupleSet.empty [] warnings

let ( >>= ) i f = if i = 0 then f () else i

let compare_gen_pers_event =
  Def_ord.compare_gen_pers_event Gwdb.compare_iper Gwdb.compare_istr

let compare_gen_fam_event =
  Def_ord.compare_gen_fam_event Gwdb.compare_iper Gwdb.compare_istr

let compare_gen_title = Def_ord.compare_gen_title Gwdb.compare_istr

let compare_normalized_base_warning (w1 : base_warning) (w2 : base_warning) :
    int =
  match (w1, w2) with
  | PossibleDuplicateFam (f1, f2), PossibleDuplicateFam (f1', f2') ->
      Gwdb.compare_ifam f1 f1' >>= fun () -> Gwdb.compare_ifam f2 f2'
  | ( PossibleDuplicateFamHomonymous (ifam1, ifam2, _),
      PossibleDuplicateFamHomonymous (ifam1', ifam2', _) ) ->
      Gwdb.compare_ifam ifam1 ifam1' >>= fun () ->
      Gwdb.compare_ifam ifam2 ifam2'
  | BigAgeBetweenSpouses (p1, p2, d), BigAgeBetweenSpouses (p1', p2', d') ->
      compare_person p1 p1' >>= fun () ->
      compare_person p2 p2' >>= fun () -> Date.compare_dmy d d'
  | BirthAfterDeath p, BirthAfterDeath p' -> compare_person p p'
  | IncoherentSex (p, s1, s2), IncoherentSex (p', s1', s2') ->
      compare_person p p' >>= fun () ->
      s1 - s1' >>= fun () -> s2 - s2'
  | ( ChangedOrderOfChildren (ifam, fam, ipers1, ipers2),
      ChangedOrderOfChildren (ifam', fam', ipers1', ipers2') ) ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      compare_family fam fam' >>= fun () ->
      compare_array Gwdb.compare_iper ipers1 ipers1' >>= fun () ->
      compare_array Gwdb.compare_iper ipers2 ipers2'
  | ( ChangedOrderOfMarriages (p, ifams, ifams2),
      ChangedOrderOfMarriages (p', ifams', ifams2') ) ->
      compare_person p p' >>= fun () ->
      compare_array Gwdb.compare_ifam ifams ifams' >>= fun () ->
      compare_array Gwdb.compare_ifam ifams2 ifams2'
  | ( ChangedOrderOfFamilyEvents (ifam, fevents, fevents2),
      ChangedOrderOfFamilyEvents (ifam', fevents', fevents2') ) ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      Ext_list.compare compare_gen_fam_event fevents fevents' >>= fun () ->
      Ext_list.compare compare_gen_fam_event fevents2 fevents2'
  | ( ChangedOrderOfPersonEvents (p, pevents, pevents2),
      ChangedOrderOfPersonEvents (p', pevents', pevents2') ) ->
      compare_person p p' >>= fun () ->
      Ext_list.compare compare_gen_pers_event pevents pevents' >>= fun () ->
      Ext_list.compare compare_gen_pers_event pevents2 pevents2'
  | ( ChildrenNotInOrder (ifam, fam, p1, p2),
      ChildrenNotInOrder (ifam', fam', p1', p2') ) ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      compare_family fam fam' >>= fun () ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | CloseChildren (ifam, p1, p2), CloseChildren (ifam', p1', p2') ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | DeadOld (p, d), DeadOld (p', d') ->
      compare_person p p' >>= fun () -> Date.compare_dmy d d'
  | DeadTooEarlyToBeFather (p1, p2), DeadTooEarlyToBeFather (p1', p2') ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | DistantChildren (ifam, p1, p2), DistantChildren (ifam', p1', p2') ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | FEventOrder (p, fevent, fevent2), FEventOrder (p', fevent', fevent2') ->
      compare_person p p' >>= fun () ->
      compare_gen_fam_event fevent fevent' >>= fun () ->
      compare_gen_fam_event fevent2 fevent2'
  | ( FWitnessEventAfterDeath (p, fevent, ifam),
      FWitnessEventAfterDeath (p', fevent', ifam') ) ->
      compare_person p p' >>= fun () ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      compare_gen_fam_event fevent fevent'
  | ( FWitnessEventBeforeBirth (p, fevent, ifam),
      FWitnessEventBeforeBirth (p', fevent', ifam') ) ->
      compare_person p p' >>= fun () ->
      Gwdb.compare_ifam ifam ifam' >>= fun () ->
      compare_gen_fam_event fevent fevent'
  | IncoherentAncestorDate (p1, p2), IncoherentAncestorDate (p1', p2') ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | MarriageDateAfterDeath p, MarriageDateAfterDeath p' -> compare_person p p'
  | MarriageDateBeforeBirth p, MarriageDateBeforeBirth p' -> compare_person p p'
  | MotherDeadBeforeChildBirth (p1, p2), MotherDeadBeforeChildBirth (p1', p2')
    ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | ParentBornAfterChild (p1, p2), ParentBornAfterChild (p1', p2') ->
      compare_person p1 p1' >>= fun () -> compare_person p2 p2'
  | ParentTooOld (p1, d, p2), ParentTooOld (p1', d', p2') ->
      compare_person p1 p1' >>= fun () ->
      compare_person p2 p2' >>= fun () -> Date.compare_dmy d d'
  | ParentTooYoung (p1, d, p2), ParentTooYoung (p1', d', p2') ->
      compare_person p1 p1' >>= fun () ->
      compare_person p2 p2' >>= fun () -> Date.compare_dmy d d'
  | PEventOrder (p, pevent1, pevent2), PEventOrder (p', pevent1', pevent2') ->
      compare_person p p' >>= fun () ->
      compare_gen_pers_event pevent1 pevent1' >>= fun () ->
      compare_gen_pers_event pevent2 pevent2'
  | ( PWitnessEventAfterDeath (p1, pevent, p2),
      PWitnessEventAfterDeath (p1', pevent', p2') ) ->
      compare_person p1 p1' >>= fun () ->
      compare_person p2 p2' >>= fun () -> compare_gen_pers_event pevent pevent'
  | ( PWitnessEventBeforeBirth (p1, pevent, p2),
      PWitnessEventBeforeBirth (p1', pevent', p2') ) ->
      compare_person p1 p1' >>= fun () ->
      compare_person p2 p2' >>= fun () -> compare_gen_pers_event pevent pevent'
  | TitleDatesError (p, title), TitleDatesError (p', title') ->
      compare_person p p' >>= fun () -> compare_gen_title title title'
  | UndefinedSex p, UndefinedSex p' -> compare_person p p'
  | YoungForMarriage (p, d, ifam), YoungForMarriage (p', d', ifam') ->
      compare_person p p' >>= fun () ->
      Gwdb.compare_ifam ifam ifam' >>= fun () -> Date.compare_dmy d d'
  | OldForMarriage (p, d, ifam), OldForMarriage (p', d', ifam') ->
      compare_person p p' >>= fun () ->
      Gwdb.compare_ifam ifam ifam' >>= fun () -> Date.compare_dmy d d'
  | _ -> assert false (* should not happen *)

let compare_base_warning w1 w2 =
  Int.compare (int_of_warning_tag w1) (int_of_warning_tag w2) >>= fun () ->
  compare_normalized_base_warning (normalize_warning w1) (normalize_warning w2)

module BaseWarningSet = Set.Make (struct
  type t = base_warning

  let compare = compare_base_warning
end)
