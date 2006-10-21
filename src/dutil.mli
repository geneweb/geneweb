(* $Id: dutil.mli,v 5.6 2006-10-21 02:24:57 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Dbdisk;

type name_index_data = array (array Def.iper);
type strings_of_fsname = array (array dsk_istr);

value magic_gwb : string;
value magic_gwb_iso_8859_1 : string;
value table_size : int;

value check_magic : in_channel -> unit;
value compare_istr_fun : Dbdisk.base_data -> dsk_istr -> dsk_istr -> int;
value compare_names : Dbdisk.base_data -> string -> string -> int;

value dsk_person_misc_names :
  dsk_base -> dsk_person -> (dsk_person -> list dsk_title) -> list string;
value gen_person_misc_names :
  string -> string -> string -> list string -> list string -> list string ->
    list string -> list (Def.gen_title string) ->
    list (string * list string) -> list string -> list string;

value poi : dsk_base -> Def.iper -> dsk_person;
value sou : dsk_base -> dsk_istr -> string;
value p_first_name : dsk_base -> dsk_person -> string;
value p_surname : dsk_base -> dsk_person -> string;
