(* $Id: gutil.mli,v 5.19 2006-10-04 13:21:43 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Gwdb;

value open_base : string -> base;
value close_base : base -> unit;

value is_deleted_family : family -> bool;
value spouse : iper -> couple -> iper;

value split_key : string -> (string * int * string);

value person_ht_add : base -> string -> iper -> unit;
value person_ht_find_all : base -> string -> list iper;
value person_ht_find_unique : base -> string -> string -> int -> iper;
value person_of_key : base -> string -> option iper;
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

type base_error = error person;
type base_warning = warning person descend title;

value check_person :
  base -> (base_error -> unit) -> (base_warning -> unit) -> person ->
    option (list (iper * person))
;

value check_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> family ->
    couple -> descend -> unit
;

value strip_spaces : string -> string;
value gen_strip_spaces : bool -> string -> string;
value alphabetic_utf_8 : string -> string -> int;
value alphabetic : string -> string -> int;
value alphabetic_order : string -> string -> int;

value lindex : string -> char -> option int;
value list_iter_first : (bool -> 'a -> unit) -> list 'a -> unit;

value arg_list_of_string : string -> list string;

value sort_person_list : base -> list person -> list person;

value father : gen_couple 'a -> 'a;
value mother : gen_couple 'a -> 'a;
value couple : bool -> 'a -> 'a -> gen_couple 'a;
value parent : bool -> array 'a -> gen_couple 'a;
value parent_array : gen_couple 'a -> array 'a;

value no_ascend : unit -> ascend;

value find_free_occ : base -> string -> string -> int -> int;

value input_lexicon :
  string -> Hashtbl.t string string -> (unit -> in_channel) -> unit;
