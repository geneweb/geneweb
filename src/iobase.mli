(* $Id: iobase.mli,v 5.7 2006-10-02 14:39:01 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;

type name_index_data = array (array iper);
type strings_of_fsname = array (array istr);

value input : string -> Dbdisk.dsk_base;

value check_magic : in_channel -> unit;
value magic_gwb : string;
value magic_gwb_iso_8859_1 : string;
value table_size : int;
value compare_istr : Dbdisk.dsk_base -> istr -> istr -> int;
