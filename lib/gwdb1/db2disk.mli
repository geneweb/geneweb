(* $Id: db2disk.mli,v 5.10 2007-03-03 05:27:21 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def

type patches =
  { mutable nb_per : int;
    mutable nb_fam : int;
    nb_per_ini : int;
    nb_fam_ini : int;
    h_person : (iper, (iper, string) gen_person) Hashtbl.t;
    h_ascend : (iper, ifam gen_ascend) Hashtbl.t;
    h_union : (iper, ifam gen_union) Hashtbl.t;
    h_family : (ifam, (iper, string) gen_family) Hashtbl.t;
    h_couple : (ifam, iper gen_couple) Hashtbl.t;
    h_descend : (ifam, iper gen_descend) Hashtbl.t;
    h_key : (string * string * int, iper option) Hashtbl.t;
    h_name : (string, iper list) Hashtbl.t }

type db2 =
  { phony : unit -> unit;
    bdir2 : string;
    cache_chan : (string * string * string, in_channel) Hashtbl.t;
    patches : patches;
    mutable parents_array : ifam option array option;
    mutable consang_array : Adef.fix array option;
    mutable family_array : ifam array array option;
    mutable father_array : iper array option;
    mutable mother_array : iper array option;
    mutable children_array : iper array array option }

type string_person =
    Sp of int
  | SpNew of string

type string_person_index2 =
  { is_first_name : bool;
    index_of_first_char : (string * int) list;
    mutable ini : string;
    mutable curr_i : int;
    mutable curr_s : string }

val field_exists : db2 -> string * string -> bool
val get_field_acc : db2 -> int -> string * string -> int
val get_field_data : db2 -> int -> string * string -> string -> 'a
val get_field_2_data : db2 -> int -> string * string -> string -> 'a * 'b
val get_field : db2 -> int -> string * string -> 'a
val string_of_istr2 : db2 -> string * string -> int -> string

val spi2_first :
  db2 -> string_person_index2 -> string * string -> string -> string_person
val spi2_next :
  db2 -> string_person_index2 -> string * string -> bool ->
    string_person * int
val spi2_find :
  db2 -> string_person_index2 -> string * string -> int -> iper list
val spi2gen_find : db2 -> string_person_index2 -> string -> iper list

val disk_person2_of_key : db2 -> string -> string -> int -> iper option
val person2_of_key : db2 -> string -> string -> int -> iper option
val strings2_of_fsname : db2 -> string -> string -> int list
val persons2_of_name : db2 -> string -> iper list
val persons_of_first_name_or_surname2 : db2 -> bool -> string_person_index2

val load_couples_array2 : db2 -> unit

val parents_array2 : db2 -> int -> int -> ifam option array
val consang_array2 : db2 -> int -> Adef.fix array
val family_array2 : db2 -> ifam array array
val children_array2 : db2 -> iper array array
val read_notes : db2 -> string -> rn_mode -> string

val commit_patches2 : db2 -> unit
val commit_notes2 : db2 -> string -> string -> unit
val base_of_base2 : string -> db2

val iter_patched_keys :
  db2 -> (string * string * int -> iper option -> unit) -> unit
