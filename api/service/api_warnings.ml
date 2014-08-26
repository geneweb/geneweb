(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)


module M = Api_piqi
module Mext = Api_piqi_ext

open Config
open Def
open Gwdb
open Api_def
open Api_util



let set_list l v = l := v :: !l ;;

(* Listes des erreurs *)
let already_defined = ref [] ;;
let own_ancestor = ref [] ;;
let bad_sex_of_married_person = ref [];;

(* Listes des warnings *)
let big_age_between_spouses = ref [] ;;
let birth_after_death = ref [] ;;
let incoherent_sex = ref [] ;;
let changed_order_of_children = ref [] ;;
let changed_order_of_marriages = ref [] ;;
let changed_order_of_family_events = ref [] ;;
let changed_order_of_person_events = ref [] ;;
let children_not_in_order = ref [] ;;
let close_children = ref [] ;;
let dead_old = ref [] ;;
let dead_too_early_to_be_father = ref [] ;;
let fevent_order = ref [] ;;
let fevent_witness_after_death = ref [] ;;
let fevent_witness_before_birth = ref [] ;;
let incoherent_ancestor_date = ref [] ;;
let marriage_date_after_death = ref [] ;;
let marriage_date_before_birth = ref [] ;;
(* Renommage : MotherDeadAfterChildBirth => mother_dead_before_child_birth *)
let mother_dead_before_child_birth = ref [] ;;
let parent_born_after_child = ref [] ;;
let parent_too_old = ref [] ;;
let parent_too_young = ref [] ;;
let pevent_order = ref [] ;;
let pevent_witness_after_death = ref [] ;;
let pevent_witness_before_birth = ref [] ;;
let title_dates_error = ref [] ;;
let undefined_sex = ref [] ;;
let witness_date_after_death = ref [] ;;
let witness_date_before_birth = ref [] ;;
let young_for_marriage = ref [] ;;
let old_individual = ref [] ;;



(* ********************************************************************* *)
(*  [Fonc] add_error_to_piqi_warning_list : 
             config -> base -> bool -> person error -> unit              *)
(** [Description] : Fonction qui ajoute une erreur dans la liste des 
                    "piqi warning" correspondante.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - base_loop : liste de personnes
      - error     :
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let add_error_to_piqi_warning_list conf base base_loop compute_sosa load_img error =
  match error with
  | AlreadyDefined p -> 
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_already_defined#{person = p} in
      set_list already_defined w
  | OwnAncestor p -> 
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_own_ancestor#{person = p} in
      set_list own_ancestor w
  | BadSexOfMarriedPerson p ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_bad_sex_of_married_person#{person = p} in
      set_list bad_sex_of_married_person w
;;


(* ********************************************************************* *)
(*  [Fonc] add_warning_to_piqi_warning_list : 
             config -> base -> bool -> (person, _, _) warning -> unit    *)
(** [Description] : Fonction qui ajoute un warning dans la liste des 
                    "piqi warning" correspondante.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - base_loop : liste de personnes
      - warning   :
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let add_warning_to_piqi_warning_list conf base base_loop compute_sosa load_img warning =
  match warning with
  | BigAgeBetweenSpouses (fath, moth, dmy) ->
      let father = 
        pers_to_piqi_person_full conf base fath base_loop compute_sosa load_img
      in 
      let mother = 
        pers_to_piqi_person_full conf base moth base_loop compute_sosa load_img
      in 
      let date = string_of_prec_dmy conf dmy in
      let w = 
        M.Warning_big_age_between_spouses#{
          father = father;
          mother = mother;
          date = date;
        }
      in
      set_list big_age_between_spouses w
  | BirthAfterDeath p ->
      let p = pers_to_piqi_person_full conf base p base_loop compute_sosa load_img in
      let w = M.Warning_birth_after_death#{person = p} in
      set_list birth_after_death w
  | IncoherentSex (p, _, _) ->
      let p = pers_to_piqi_person_full conf base p base_loop compute_sosa load_img in
      let w = 
        M.Warning_incoherent_sex#{
          person = p;
        } 
      in
      set_list incoherent_sex w
  | ChangedOrderOfChildren (ifam, _, _, _) ->
      let cpl = foi base ifam in
      let f = poi base (get_father cpl) in
      let m = poi base (get_mother cpl) in
      let father = 
        pers_to_piqi_person_full conf base f base_loop compute_sosa load_img
      in
      let mother =
        pers_to_piqi_person_full conf base m base_loop compute_sosa load_img
      in
      let w = 
        M.Warning_changed_order_of_children#{
          father = father; 
          mother = mother;
        }
      in
      set_list changed_order_of_children w
  | ChangedOrderOfMarriages (p, _, _) ->
      let p =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w =
        M.Warning_changed_order_of_marriages#{
          person = p;
        }
      in
      set_list changed_order_of_marriages w
  | ChangedOrderOfFamilyEvents (ifam, _, _) ->
      let cpl = foi base ifam in
      let f = poi base (get_father cpl) in
      let m = poi base (get_mother cpl) in
      let father =
        pers_to_piqi_person_full conf base f base_loop compute_sosa load_img
      in
      let mother =
        pers_to_piqi_person_full conf base m base_loop compute_sosa load_img
      in
      let w =
        M.Warning_changed_order_of_family_events#{
          father = father;
          mother = mother;
        }
      in
      set_list changed_order_of_family_events w
  | ChangedOrderOfPersonEvents (p, _, _) ->
      let p =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w =
        M.Warning_changed_order_of_person_events#{
          person = p;
        }
      in
      set_list changed_order_of_person_events w
  | ChildrenNotInOrder (ifam, _, _, _) ->
      let cpl = foi base ifam in
      let f = poi base (get_father cpl) in
      let m = poi base (get_mother cpl) in
      let father = 
        pers_to_piqi_person_full conf base f base_loop compute_sosa load_img
      in
      let mother =
        pers_to_piqi_person_full conf base m base_loop compute_sosa load_img
      in
      let w = 
        M.Warning_children_not_in_order#{
          father = father; 
          mother = mother;
        }
      in
      set_list children_not_in_order w
  | CloseChildren (ifam, _, c1, c2) ->
      let cpl = foi base ifam in
      let f = poi base (get_father cpl) in
      let m = poi base (get_mother cpl) in
      let father = 
        pers_to_piqi_person_full conf base f base_loop compute_sosa load_img
      in
      let mother =
        pers_to_piqi_person_full conf base m base_loop compute_sosa load_img
      in
      let child1 = 
        pers_to_piqi_person_full conf base c1 base_loop compute_sosa load_img
      in
      let child2 = 
        pers_to_piqi_person_full conf base c2 base_loop compute_sosa load_img
      in
      let w = 
        M.Warning_close_children#{
          father = father; 
          mother = mother;
          child1 = child1;
          child2 = child2;
        }
      in
      set_list close_children w
  | DeadOld (p, dmy) ->
      let p = pers_to_piqi_person_full conf base p base_loop compute_sosa load_img in
      let date = string_of_prec_dmy conf dmy in
      let w = 
        M.Warning_dead_old#{
          person = p; 
          date = date;
        }
      in
      set_list dead_old w
  | DeadTooEarlyToBeFather (f, s) ->
      let father = 
        pers_to_piqi_person_full conf base f base_loop compute_sosa load_img
      in
      let son =
        pers_to_piqi_person_full conf base s base_loop compute_sosa load_img
      in
      let w = 
        M.Warning_dead_too_early_to_be_father#{
          father = father; 
          son = son;
        }
      in
      set_list dead_too_early_to_be_father w
  | FEventOrder (p, e1, e2) ->
      let p = pers_to_piqi_person_full conf base p base_loop compute_sosa load_img in
      let e1 = Util.string_of_fevent_name conf base e1.efam_name in
      let e2 = Util.string_of_fevent_name conf base e2.efam_name in
      let w =
        M.Warning_fevent_order#{
          person = p;
          event1 = e1;
          event2 = e2;
        }
      in
      set_list fevent_order w
  | FWitnessEventAfterDeath (p, e) ->
      let p =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let e = Util.string_of_fevent_name conf base e.efam_name in
      let w =
        M.Warning_fwitness_event_after_death#{
          person = p;
          event = e;
        }
      in
      set_list fevent_witness_after_death w
  | FWitnessEventBeforeBirth (p, e) ->
      let p =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let e = Util.string_of_fevent_name conf base e.efam_name in
      let w =
        M.Warning_fwitness_event_before_birth#{
          person = p;
          event = e;
        }
      in
      set_list fevent_witness_before_birth w
  | IncoherentAncestorDate (a, p) ->
      let ancestor = 
        pers_to_piqi_person_full conf base a base_loop compute_sosa load_img
      in
      let person =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = 
        M.Warning_incoherent_ancestor_date#{
          person = person; 
          ancestor = ancestor;
        }
      in
      set_list incoherent_ancestor_date w
  | MarriageDateAfterDeath p ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_marriage_date_after_death#{person = p} in
      set_list marriage_date_after_death w
  | MarriageDateBeforeBirth p ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_marriage_date_before_birth#{person = p} in
      set_list marriage_date_before_birth w
  | MotherDeadAfterChildBirth (m, c) ->
      let mother = 
        pers_to_piqi_person_full conf base m base_loop compute_sosa load_img
      in
      let child =
        pers_to_piqi_person_full conf base c base_loop compute_sosa load_img
      in
      (* Étrangement c'est le seul message qui se lit de droite à gauche *)
      (* Naissance de l'enfant après la mort de sa mère => on le renomme *)
      let w = 
        M.Warning_mother_dead_before_child_birth#{
          mother = mother; 
          child = child;
        }
      in
      set_list mother_dead_before_child_birth w
  | ParentBornAfterChild (p, c) ->
      let parent = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let child =
        pers_to_piqi_person_full conf base c base_loop compute_sosa load_img
      in
      let w = 
        M.Warning_parent_born_after_child#{
          parent = parent; 
          child = child;
        } 
      in
      set_list parent_born_after_child w
  | ParentTooOld (p, dmy) ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let date = string_of_prec_dmy conf dmy in
      let w = 
        M.Warning_parent_too_old#{
          parent = p; 
          date = date;
        } 
      in
      set_list parent_too_old w
  | ParentTooYoung (p, dmy) ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let date = string_of_prec_dmy conf dmy in
      let w = 
        M.Warning_parent_too_young#{
          parent = p; 
          date = date;
        } 
      in
      set_list parent_too_young w
  | PEventOrder (p, e1, e2) ->
      let p = pers_to_piqi_person_full conf base p base_loop compute_sosa load_img in
      let e1 = Util.string_of_pevent_name conf base e1.epers_name in
      let e2 = Util.string_of_pevent_name conf base e2.epers_name in
      let w =
        M.Warning_pevent_order#{
          person = p;
          event1 = e1;
          event2 = e2;
        }
      in
      set_list pevent_order w
  | PWitnessEventAfterDeath (p, e) ->
      let p =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let e = Util.string_of_pevent_name conf base e.epers_name in
      let w =
        M.Warning_pwitness_event_after_death#{
          person = p;
          event = e;
        }
      in
      set_list pevent_witness_after_death w
  | PWitnessEventBeforeBirth (p, e) ->
      let p =
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let e = Util.string_of_pevent_name conf base e.epers_name in
      let w =
        M.Warning_pwitness_event_before_birth#{
          person = p;
          event = e;
        }
      in
      set_list pevent_witness_before_birth w
  | TitleDatesError (p, _) ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_title_dates_error#{person = p} in
      set_list title_dates_error w
  | UndefinedSex p ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_undefined_sex#{person = p} in
      set_list undefined_sex w
  | WitnessDateAfterDeath p ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_witness_date_after_death#{person = p} in
      set_list witness_date_after_death w
  | WitnessDateBeforeBirth p ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let w = M.Warning_witness_date_before_birth#{person = p} in
      set_list witness_date_before_birth w
  | YoungForMarriage (p, dmy) ->
      let p = 
        pers_to_piqi_person_full conf base p base_loop compute_sosa load_img
      in
      let date = string_of_prec_dmy conf dmy in
      let w = 
        M.Warning_young_for_marriage#{
          person = p; 
          date = date;
        } 
      in
      set_list young_for_marriage w
;;

let create_piqi_warnings () =
  (* Ajouter une limite. Pour pierfit, on peut 
     exploser la taille des données à envoyer. *)
  M.Base_warnings#{
    warning_already_defined = !already_defined;
    warning_own_ancestor = !own_ancestor;
    warning_bad_sex_of_married_person = !bad_sex_of_married_person;
    warning_birth_after_death = !birth_after_death;
    warning_incoherent_sex = !incoherent_sex;
    warning_changed_order_of_children = !changed_order_of_children;
    warning_children_not_in_order = !children_not_in_order;
    warning_dead_too_early_to_be_father = !dead_too_early_to_be_father;
    warning_incoherent_ancestor_date = !incoherent_ancestor_date;
    warning_marriage_date_after_death = !marriage_date_after_death;
    warning_marriage_date_before_birth = !marriage_date_before_birth;
    warning_mother_dead_before_child_birth = !mother_dead_before_child_birth;
    warning_parent_born_after_child = !parent_born_after_child;
    warning_parent_too_young = !parent_too_young;
    warning_title_dates_error = !title_dates_error;
    warning_undefined_sex = !undefined_sex;
    warning_young_for_marriage = !young_for_marriage;
    warning_close_children = !close_children;
    warning_parent_too_old = !parent_too_old;
    warning_changed_order_of_marriages = !changed_order_of_marriages;
    warning_big_age_between_spouses = !big_age_between_spouses;
    warning_dead_old = !dead_old;
    warning_old_individual = !old_individual;
    warning_witness_date_after_death = !witness_date_after_death;
    warning_witness_date_before_birth = !witness_date_before_birth;
  }
;;
