(* $Id: db2disk.mli,v 5.10 2007-03-03 05:27:21 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def;

type patches =
  { nb_per : mutable int;
    nb_fam : mutable int;
    nb_per_ini : int;
    nb_fam_ini : int;
    h_person : Hashtbl.t iper (gen_person iper string);
    h_ascend : Hashtbl.t iper (gen_ascend ifam);
    h_union : Hashtbl.t iper (gen_union ifam);
    h_family : Hashtbl.t ifam (gen_family iper string);
    h_couple : Hashtbl.t ifam (gen_couple iper);
    h_descend : Hashtbl.t ifam (gen_descend iper);
    h_key : Hashtbl.t (string * string * int) (option iper);
    h_name : Hashtbl.t string (list iper) }
;

type db2 =
  { phony : unit -> unit;
    bdir2 : string;
    cache_chan : Hashtbl.t (string * string * string) in_channel;
    patches : patches;
    parents_array : mutable option (array (option ifam));
    consang_array : mutable option (array Adef.fix);
    family_array : mutable option (array (array ifam));
    father_array : mutable option (array iper);
    mother_array : mutable option (array iper);
    children_array : mutable option (array (array iper)) }
;

type string_person =
  [ Sp of int
  | SpNew of string ]
;

type string_person_index2 =
  { is_first_name : bool;
    index_of_first_char : list (string * int);
    ini : mutable string;
    curr_i : mutable int;
    curr_s : mutable string }
;

value field_exists : db2 -> (string * string) -> bool;
value get_field_acc : db2 -> int -> (string * string) -> int;
value get_field_data :
  db2 -> int -> (string * string) -> string -> 'a;
value get_field_2_data :
  db2 -> int -> (string * string) -> string -> ('a * 'b);
value get_field : db2 -> int -> (string * string) -> 'a;
value string_of_istr2 : db2 -> (string * string) -> int -> string;

value spi2_first :
  db2 -> string_person_index2 -> (string * string) -> string -> string_person;
value spi2_next :
  db2 -> string_person_index2 -> (string * string) -> bool ->
    (string_person * int);
value spi2_find :
  db2 -> string_person_index2 -> (string * string) -> int -> list iper;
value spi2gen_find :
  db2 -> string_person_index2 -> string -> list iper;

value disk_person2_of_key : db2 -> string -> string -> int -> option iper;
value person2_of_key : db2 -> string -> string -> int -> option iper;
value strings2_of_fsname : db2 -> string -> string -> list int;
value persons2_of_name : db2 -> string -> list iper;
value persons_of_first_name_or_surname2 : db2 -> bool -> string_person_index2;

value load_couples_array2 : db2 -> unit;

value parents_array2 : db2 -> int -> int -> array (option ifam);
value consang_array2 : db2 -> int -> array Adef.fix;
value family_array2 : db2 -> array (array ifam);
value children_array2 : db2 -> array (array iper);
value read_notes : db2 -> string -> rn_mode -> string;

value commit_patches2 : db2 -> unit;
value commit_notes2 : db2 -> string -> string -> unit;
value base_of_base2 : string -> db2;

value iter_patched_keys :
  db2 -> ((string * string * int) -> option iper -> unit) -> unit;
