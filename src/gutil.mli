(* $Id: gutil.mli,v 3.9 2000-11-11 12:50:49 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;

type choice 'a 'b = [ Left of 'a | Right of 'b ];

value poi : base -> iper -> person;
value aoi : base -> iper -> ascend;
value uoi : base -> iper -> union;

value foi : base -> ifam -> family;
value coi : base -> ifam -> couple;
value doi : base -> ifam -> descend;

value sou : base -> istr -> string;

value decline : char -> string -> string;
value nominative : string -> string;

value p_first_name : base -> person -> string;
value p_surname : base -> person -> string;

value is_deleted_family : family -> bool;
value spouse : iper -> couple -> iper;

value person_ht_add : base -> string -> iper -> unit;
value person_ht_find_all : base -> string -> list iper;
value person_ht_find_unique : base -> string -> string -> int -> iper;
value person_misc_names : base -> person -> list string;

value leap_year : int -> bool;
value nb_jours_dans_mois : int -> int -> int;
value temps_ecoule : dmy -> dmy -> dmy;
value annee : dmy -> int;
value strictement_avant_dmy : dmy -> dmy -> bool;
value strictement_apres_dmy : dmy -> dmy -> bool;
value strictement_avant : date -> date -> bool;
value strictement_apres : date -> date -> bool;
value date_of_death : death -> option date;
value roman_of_arabian : int -> string;

value denomination : base -> person -> string;

value map_title_strings : ('a -> 'b) -> gen_title 'a -> gen_title 'b;
value map_relation_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_relation 'a 'b -> gen_relation 'c 'd
;
value map_person_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_person 'a 'b -> gen_person 'c 'd
;
value map_family_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_family 'a 'b -> gen_family 'c 'd
;
value map_couple_p : ('a -> 'b) -> gen_couple 'a -> gen_couple 'b;
value map_descend_p : ('a -> 'b) -> gen_descend 'a -> gen_descend 'b;

(* check base *)

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;
type base_error = error person;

type warning 'person =
  [ BirthAfterDeath of 'person
  | IncoherentSex of 'person
  | ChangedOrderOfChildren of ifam and descend and array iper
  | ChildrenNotInOrder of ifam and descend and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and dmy
  | TitleDatesError of 'person and title
  | YoungForMarriage of 'person and dmy ]
;
type base_warning = warning person;

value check_person :
  base -> (base_error -> unit) -> (base_warning -> unit) -> person -> unit
;

value check_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> family ->
    couple -> descend -> unit
;

value check_noloop : base -> (base_error -> unit) -> unit;
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
value array_memq : 'a -> array 'a -> bool;
value list_iter_first : (bool -> 'a -> unit) -> list 'a -> unit;

value surnames_pieces : string -> list string;

value arg_list_of_string : string -> list string;
