#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

open Geneweb
open Def
open Gwdb
open Api_util

let empty : M.Base_warnings.t =
  { warning_already_defined = []
  ; warning_own_ancestor = []
  ; warning_bad_sex_of_married_person = []
  ; warning_big_age_between_spouses = []
  ; warning_birth_after_death = []
  ; warning_incoherent_sex = []
  ; warning_changed_order_of_children = []
  ; warning_changed_order_of_marriages = []
  ; warning_children_not_in_order = []
  ; warning_close_children = []
  ; warning_dead_old = []
  ; warning_dead_too_early_to_be_father = []
  ; warning_incoherent_ancestor_date = []
  ; warning_marriage_date_after_death = []
  ; warning_marriage_date_before_birth = []
  ; warning_mother_dead_before_child_birth = []
  ; warning_parent_born_after_child = []
  ; warning_parent_too_old = []
  ; warning_parent_too_young = []
  ; warning_possible_duplicate_fam = []
  ; warning_title_dates_error = []
  ; warning_undefined_sex = []
  ; warning_witness_date_after_death = []
  ; warning_witness_date_before_birth = []
  ; warning_young_for_marriage = []
  ; warning_old_for_marriage = []
  ; warning_distant_children = []
  ; warning_event_order = []
  }

(** [add_error_to_piqi_warning_list base error]
    Convert [error] and add it to corresponding error list
*)
let add_error_to_piqi_warning_list base (w : M.Base_warnings.t) = function
  | AlreadyDefined p ->
    { w with warning_already_defined =
               M.Warning_already_defined.{person = person_to_warning_person base p }
               :: w.warning_already_defined }
  | OwnAncestor p ->
    { w with warning_own_ancestor =
               M.Warning_own_ancestor.{person = person_to_warning_person base p }
               :: w.warning_own_ancestor }
  | BadSexOfMarriedPerson p ->
    { w with warning_bad_sex_of_married_person =
               M.Warning_bad_sex_of_married_person.{person = person_to_warning_person base p }
               :: w.warning_bad_sex_of_married_person }

let fevent_to_warning_event e =
  { M.Warning_event.pevent = None
  ; fevent =
      try Some (Api_piqi_util.piqi_fevent_name_of_fevent_name e.efam_name)
      with _ -> None
  }

let pevent_to_warning_event e =
  { M.Warning_event.fevent = None
  ; pevent =
      try Some (Api_piqi_util.piqi_pevent_name_of_pevent_name e.epers_name)
      with _ -> None
  }

let add_warning_to_piqi_warning_list conf base =
  let p2wp = person_to_warning_person in
  fun (w : M.Base_warnings.t) -> function
    | BigAgeBetweenSpouses (fath, moth, dmy) ->
      { w with warning_big_age_between_spouses =
                 M.Warning_big_age_between_spouses.{
                   father = p2wp base fath
                 ; mother = p2wp base moth
                 ; date = string_of_prec_dmy dmy
                 } :: w.warning_big_age_between_spouses }
    | BirthAfterDeath p ->
      { w with warning_birth_after_death =
                 M.Warning_birth_after_death.{ person = p2wp base p }
                 :: w.warning_birth_after_death }
    | IncoherentSex (p, _, _) ->
      { w with warning_incoherent_sex =
                 M.Warning_incoherent_sex.{ person = p2wp base p }
                 :: w.warning_incoherent_sex }
    | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = foi base ifam in
      { w with warning_changed_order_of_children =
                 M.Warning_changed_order_of_children.{
                   father = p2wp base @@ poi base @@ get_father cpl
                 ; mother = p2wp base @@ poi base @@ get_mother cpl
                 } :: w.warning_changed_order_of_children }
    | ChangedOrderOfMarriages (p, _, _) ->
      { w with warning_changed_order_of_marriages =
                 M.Warning_changed_order_of_marriages.{ person = p2wp base p }
                 :: w.warning_changed_order_of_marriages }
    | ChildrenNotInOrder (ifam, _, _, _) ->
      let cpl = foi base ifam in
      { w with warning_children_not_in_order =
                 M.Warning_children_not_in_order.{
                   father = p2wp base @@ poi base @@ get_father cpl
                 ; mother = p2wp base @@ poi base @@ get_mother cpl
                 } :: w.warning_children_not_in_order }
    | CloseChildren (ifam, c1, c2) ->
      let cpl = foi base ifam in
      { w with warning_close_children =
                 M.Warning_close_children.{
                   father = p2wp base @@ poi base @@ get_father cpl
                 ; mother = p2wp base @@ poi base @@ get_mother cpl
                 ; child1 = p2wp base c1
                 ; child2 = p2wp base c2
                 } :: w.warning_close_children }
    | DeadOld (p, dmy) ->
      { w with warning_dead_old =
                 M.Warning_dead_old.{
                   person = p2wp base p
                 ; date = string_of_prec_dmy dmy ;
                 } :: w.warning_dead_old }
    | DeadTooEarlyToBeFather (f, s) ->
      { w with warning_dead_too_early_to_be_father =
                 M.Warning_dead_too_early_to_be_father.{
                   father = p2wp base f
                 ; son = p2wp base s;
                 } :: w.warning_dead_too_early_to_be_father }
    | DistantChildren (ifam, c1, c2) ->
      let cpl = foi base ifam in
      { w with warning_distant_children =
                 M.Warning_distant_children.{
                   father = p2wp base @@ poi base @@ get_father cpl
                 ; mother = p2wp base @@ poi base @@ get_mother cpl
                 ; child1 = p2wp base c1
                 ; child2 = p2wp base c2
                 } :: w.warning_distant_children }
    | FWitnessEventAfterDeath (p, e, ifam) ->
      let cpl = foi base ifam in
      { w with warning_witness_date_after_death =
                 M.Warning_witness_date_after_death.{
                   person = p2wp base p
                 ; event = fevent_to_warning_event e
                 ; origin = [ p2wp base @@ poi base @@ get_father cpl
                            ; p2wp base @@ poi base @@ get_mother cpl ]
                 } :: w.warning_witness_date_after_death }
    | FWitnessEventBeforeBirth (p, e, ifam) ->
      let cpl = foi base ifam in
      { w with warning_witness_date_before_birth =
                 M.Warning_witness_date_before_birth.{
                   person = p2wp base p
                 ; event = fevent_to_warning_event e
                 ; origin = [ p2wp base @@ poi base @@ get_father cpl
                            ; p2wp base @@ poi base @@ get_mother cpl ]
                 } :: w.warning_witness_date_before_birth }
    | IncoherentAncestorDate (a, p) ->
      { w with warning_incoherent_ancestor_date =
                 M.Warning_incoherent_ancestor_date.{
                   person = p2wp base p
                 ; ancestor = p2wp base a
                 } :: w.warning_incoherent_ancestor_date }
    | MarriageDateAfterDeath p ->
      { w with warning_marriage_date_after_death =
                 M.Warning_marriage_date_after_death.{person = p2wp base p}
                 :: w.warning_marriage_date_after_death }
    | MarriageDateBeforeBirth p ->
      { w with warning_marriage_date_before_birth =
                 M.Warning_marriage_date_before_birth.{person = p2wp base p}
                 :: w.warning_marriage_date_before_birth }
    | MotherDeadBeforeChildBirth (m, c) ->
      { w with warning_mother_dead_before_child_birth =
                 M.Warning_mother_dead_before_child_birth.{
                   mother = p2wp base m
                 ; child = p2wp base c
                 } :: w.warning_mother_dead_before_child_birth }
    | ParentBornAfterChild (p, c) ->
      { w with warning_parent_born_after_child =
                 M.Warning_parent_born_after_child.{
                   parent = p2wp base p
                 ; child = p2wp base c
                 } :: w.warning_parent_born_after_child }
    | ParentTooOld (p, dmy, c) ->
      { w with warning_parent_too_old =
                 M.Warning_parent_too_old.{
                   parent = p2wp base p
                 ; date = string_of_prec_dmy dmy
                 ; child = p2wp base c
                 } :: w.warning_parent_too_old }
    | ParentTooYoung (p, dmy, c) ->
      { w with warning_parent_too_young =
                 M.Warning_parent_too_young.{
                   parent = p2wp base p
                 ; date = string_of_prec_dmy dmy
                 ; child = p2wp base c
                 } :: w.warning_parent_too_young }
    | PossibleDuplicateFam (f1, f2) ->
      { w with warning_possible_duplicate_fam =
                 M.Warning_possible_duplicate_fam.{
                   family1 = fam_to_piqi_family conf base f1
                 ; family2 = fam_to_piqi_family conf base f2
                 } :: w.warning_possible_duplicate_fam }
    | PWitnessEventAfterDeath (p, e, origin) ->
      { w with warning_witness_date_after_death =
                 M.Warning_witness_date_after_death.{
                   person = p2wp base p
                 ; event = pevent_to_warning_event e
                 ; origin = [ p2wp base origin ]
                 } :: w.warning_witness_date_after_death }
    | PWitnessEventBeforeBirth (p, e, origin) ->
      { w with warning_witness_date_before_birth =
                 M.Warning_witness_date_before_birth.{
                   person = p2wp base p
                 ; event = pevent_to_warning_event e
                 ; origin = [ p2wp base origin ]
                 } :: w.warning_witness_date_before_birth }
    | TitleDatesError (p, t) ->
      let t = Futil.map_title_strings (sou base) t in
      { w with warning_title_dates_error =
                 M.Warning_title_dates_error.{ person = p2wp base p
                                             ; title = Api_util.title_to_piqi_title t }
                 :: w.warning_title_dates_error }
    | UndefinedSex p ->
      { w with warning_undefined_sex =
                 M.Warning_undefined_sex.{ person = p2wp base p }
                 :: w.warning_undefined_sex }
    | YoungForMarriage (p, dmy, _) ->
      { w with warning_young_for_marriage =
                 M.Warning_young_for_marriage.{
                   person = p2wp base p
                 ; date = string_of_prec_dmy dmy
                 } :: w.warning_young_for_marriage }
    | OldForMarriage (p, dmy, _) ->
      { w with warning_old_for_marriage =
                 M.Warning_old_for_marriage.{
                   person = p2wp base p
                 ; date = string_of_prec_dmy dmy
                 } :: w.warning_old_for_marriage }
    | FEventOrder (p, e1, e2) ->
      { w with warning_event_order =
                 M.Warning_event_order.{
                   person = p2wp base p
                 ; pevents = []
                 ; fevents = [ Api_piqi_util.piqi_fevent_name_of_fevent_name e1.efam_name
                             ; Api_piqi_util.piqi_fevent_name_of_fevent_name e2.efam_name ]
                 } :: w.warning_event_order }
    | PEventOrder (p, e1, e2) ->
      { w with warning_event_order =
                 M.Warning_event_order.{
                   person = p2wp base p
                 ; pevents = [ Api_piqi_util.piqi_pevent_name_of_pevent_name e1.epers_name
                             ; Api_piqi_util.piqi_pevent_name_of_pevent_name e2.epers_name ]
                 ; fevents = []
                 } :: w.warning_event_order }
    (* Not included in api *)
    | ChangedOrderOfFamilyEvents (_, _, _) -> w
    | ChangedOrderOfPersonEvents (_, _, _) -> w

#endif
