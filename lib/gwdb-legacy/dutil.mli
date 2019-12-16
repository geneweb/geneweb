(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk

type name_index_data = int array array
type strings_of_fsname = int array array

val magic_gwb : string
val magic_gwb_iso_8859_1 : string
val table_size : int

val check_magic : in_channel -> unit
val compare_istr_fun : Dbdisk.base_data -> int -> int -> int
val compare_names : Dbdisk.base_data -> string -> string -> int

val dsk_person_misc_names :
  dsk_base -> dsk_person -> (dsk_person -> dsk_title list) -> string list

val poi : dsk_base -> int -> dsk_person
val sou : dsk_base -> int -> string
val p_first_name : dsk_base -> dsk_person -> string
val p_surname : dsk_base -> dsk_person -> string

val output_value_no_sharing : out_channel -> _ -> unit
val output_array_no_sharing : out_channel -> (int -> _) -> int -> unit
val int_size : int
