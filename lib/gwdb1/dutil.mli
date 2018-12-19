(* $Id: dutil.mli,v 5.8 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Dbdisk
open Type

type name_index_data = iper array array
type strings_of_fsname = istr array array

val magic_gwb : string
val magic_gwb_iso_8859_1 : string
val table_size : int

val check_magic : in_channel -> unit
val compare_istr_fun : Dbdisk.base_data -> istr -> istr -> int
val compare_names : Dbdisk.base_data -> string -> string -> int

val dsk_person_misc_names :
  dsk_base -> dsk_person -> (dsk_person -> dsk_title list) -> string list

val poi : dsk_base -> iper -> dsk_person
val sou : dsk_base -> istr -> string
val p_first_name : dsk_base -> dsk_person -> string
val p_surname : dsk_base -> dsk_person -> string
