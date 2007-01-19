(* $Id: gutil.mli,v 5.34 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gwdb;

value spouse : iper -> family -> iper;

value person_ht_add : base -> string -> iper -> unit;
value person_not_a_key_find_all : base -> string -> list iper;
value person_ht_find_all : base -> string -> list iper;
value person_of_string_key : base -> string -> option iper;
value find_same_name : base -> person -> list person;

value designation : base -> person -> string;

value strip_spaces : string -> string;
value gen_strip_spaces : bool -> string -> string;
value alphabetic_utf_8 : string -> string -> int;
value alphabetic : string -> string -> int;
value alphabetic_order : string -> string -> int;

value arg_list_of_string : string -> list string;

value sort_person_list : base -> list person -> list person;

value father : gen_couple 'a -> 'a;
value mother : gen_couple 'a -> 'a;
value couple : bool -> 'a -> 'a -> gen_couple 'a;
value parent_array : gen_couple 'a -> array 'a;

value find_free_occ : base -> string -> string -> int -> int;
