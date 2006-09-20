(* $Id: gutil.mli,v 5.7 2006-09-20 11:30:01 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Gwdb;

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

value split_key : string -> (string * int * string);

value person_ht_add : base -> string -> iper -> unit;
value person_ht_find_all : base -> string -> list iper;
value person_ht_find_unique : base -> string -> string -> int -> iper;
value person_of_key : base -> string -> option iper;
value person_misc_names :
  base -> person -> (person -> list title) -> list string
;
value find_same_name : base -> person -> list person;

value leap_year : int -> bool;
value nb_days_in_month : int -> int -> int;
value time_gone_by : dmy -> dmy -> dmy;
value year_of : dmy -> int;
value strictly_before_dmy : dmy -> dmy -> bool;
value strictly_after_dmy : dmy -> dmy -> bool;
value strictly_before : date -> date -> bool;
value strictly_after : date -> date -> bool;
value date_of_death : death -> option date;
value roman_of_arabian : int -> string;
value arabian_of_roman : string -> int;

value designation : base -> person -> string;

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
value map_couple_p : bool -> ('a -> 'b) -> gen_couple 'a -> gen_couple 'b;
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
  | IncoherentSex of 'person and int and int
  | ChangedOrderOfChildren of ifam and descend and array iper
  | ChildrenNotInOrder of ifam and descend and 'person and 'person
  | DeadTooEarlyToBeFather of 'person and 'person
  | MarriageDateAfterDeath of 'person
  | MarriageDateBeforeBirth of 'person
  | MotherDeadAfterChildBirth of 'person and 'person
  | ParentBornAfterChild of 'person and 'person
  | ParentTooYoung of 'person and dmy
  | TitleDatesError of 'person and title
  | UndefinedSex of 'person
  | YoungForMarriage of 'person and dmy ]
;
type base_warning = warning person;

value check_person :
  base -> (base_error -> unit) -> (base_warning -> unit) -> person ->
    option (list (iper * person))
;

value check_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> family ->
    couple -> descend -> unit
;

value check_noloop : base -> (base_error -> unit) -> unit;
value check_noloop_for_person_list :
  base -> (base_error -> unit) -> list iper -> unit
;

value strip_spaces : string -> string;
value gen_strip_spaces : bool -> string -> string;
value alphabetic_utf_8 : string -> string -> int;
value alphabetic : string -> string -> int;
value alphabetic_order : string -> string -> int;
value initial : string -> int;
value strip_all_trailing_spaces : string -> string;
value tr : char -> char -> string -> string;

value rindex : string -> char -> option int;
value lindex : string -> char -> option int;
value array_memq : 'a -> array 'a -> bool;
value list_iter_first : (bool -> 'a -> unit) -> list 'a -> unit;

value surnames_pieces : string -> list string;

value arg_list_of_string : string -> list string;

value sort_person_list : base -> list person -> list person;

value father : gen_couple 'a -> 'a;
value mother : gen_couple 'a -> 'a;
value couple : bool -> 'a -> 'a -> gen_couple 'a;
value parent : bool -> array 'a -> gen_couple 'a;
value parent_array : gen_couple 'a -> array 'a;

value set_father : gen_couple 'a -> 'a -> unit;
value set_mother : gen_couple 'a -> 'a -> unit;

value no_ascend : unit -> ascend;

value find_free_occ : base -> string -> string -> int -> int;

value input_lexicon :
  string -> Hashtbl.t string string -> (unit -> in_channel) -> unit;

value utf_8_of_iso_8859_1 : string -> string;
value iso_8859_1_of_utf_8 : string -> string;

value remove_file : string -> unit;
value remove_dir : string -> unit;
value mkdir_p : string -> unit;

(* for compatibility databases version <= 4.10 which are not in utf-8 *)
value utf_8_db : ref bool;
