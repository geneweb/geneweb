(* $Id: gutil.mli,v 2.6 1999-07-22 14:34:07 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

value poi : base -> iper -> person;
value aoi : base -> iper -> ascend;
value foi : base -> ifam -> family;
value coi : base -> ifam -> couple;
value sou : base -> istr -> string;

value decline : char -> string -> string;
value nominative : string -> string;

value p_first_name : base -> person -> string;
value p_surname : base -> person -> string;

value is_deleted_family : family -> bool;

value person_ht_add : base -> string -> iper -> unit;
value person_ht_find_all : base -> string -> list iper;
value person_ht_find_unique : base -> string -> string -> int -> iper;
value person_misc_names : base -> person -> list string;

value leap_year : int -> bool;
value nb_jours_dans_mois : int -> int -> int;
value temps_ecoule : date -> date -> date;
value annee : date -> int;
value strictement_avant : date -> date -> bool;
value strictement_apres : date -> date -> bool;

value denomination : base -> person -> string;

value map_title_strings : ('a -> 'b) -> gen_title 'a -> gen_title 'b;

value map_person_strings : ('a -> 'b) -> gen_person 'a -> gen_person 'b;
value map_family_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_family 'a 'b -> gen_family 'c 'd
;
value map_couple_p : ('a -> 'b) -> gen_couple 'a -> gen_couple 'b;

(* check base *)

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;
type base_error = error person;

type warning 'person =
  [ BirthAfterDeath of 'person
  | ChangedOrderOfChildren of family and array iper
  | ChildrenNotInOrder of family and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and date
  | TitleDatesError of 'person and title
  | YoungForMarriage of 'person and date ]
;
type base_warning = warning person;

value check_person :
  base -> (base_error -> unit) -> (base_warning -> unit) -> person -> unit
;

value check_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> family ->
    unit
;

value check_noloop_for_person_list :
  base -> (base_error -> unit) -> list iper -> unit
;

value check_base :
  base -> (base_error -> unit) -> (base_warning -> unit) -> unit
;

value strip_controls_m : string -> string;
value strip_spaces : string -> string;
value alphabetique : string -> string -> int;
value initiale : string -> int;

value rindex : string -> char -> option int;
value lindex : string -> char -> option int;

value surnames_pieces : string -> list string;
