(* $Id: gutil.mli,v 1.1 1998-09-01 14:32:04 ddr Exp $ *)

open Def;

value poi : base -> iper -> base_person;
value aoi : base -> iper -> base_ascend;
value foi : base -> ifam -> base_family;
value coi : base -> ifam -> base_couple;
value sou : base -> istr -> string;

value is_deleted_family : base_family -> bool;

value person_ht_add : base -> string -> iper -> unit;
value person_ht_find_all : base -> string -> list iper;
value person_ht_find_unique : base -> string -> string -> int -> iper;
value person_misc_names : base -> base_person -> list string;

value nb_jours_dans_mois : int -> int -> int;
value temps_ecoule : date -> date -> date;
value annee : date -> int;
value strictement_avant : date -> date -> bool;
value strictement_apres : date -> date -> bool;
value string_of_date : date -> string;

value denomination : base -> base_person -> string;

value map_title_strings : ('a -> 'b) -> Def.title 'a -> Def.title 'b;

value map_person_strings : ('a -> 'b) -> Def.person 'a -> Def.person 'b;
value map_family_ps :
  ('a -> 'c) -> ('b -> 'd) -> Def.family 'a 'b -> Def.family 'c 'd
;
value map_couple_p : ('a -> 'b) -> Def.couple 'a -> Def.couple 'b;

(* check base *)

type error 'person =
  [ AlreadyDefined of 'person
  | OwnAncestor of 'person
  | BadSexOfMarriedPerson of 'person ]
;
type base_error = error base_person;

type warning 'person =
  [ BirthAfterDeath of 'person
  | ChangedOrderOfChildren of base_family and array iper
  | ChildrenNotInOrder of base_family and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and Def.date
  | TitleDatesError of 'person and title istr
  | YoungForMarriage of 'person and Def.date ]
;
type base_warning = warning base_person;

value check_person :
  base -> (base_error -> unit) -> (base_warning -> unit) -> base_person -> unit
;

value check_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> base_family ->
    unit
;

value check_noloop_for_person_list :
  base -> (base_error -> unit) -> list (base_person) -> unit
;

value check_base :
  base -> (base_error -> unit) -> (base_warning -> unit) -> unit
;

value strip_controls_m : string -> string;
value strip_spaces : string -> string;
value valeur_alphabetique : char -> int;
value alphabetique : string -> string -> int;
value initiale : string -> int;
