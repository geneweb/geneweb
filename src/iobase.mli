(* $Id: iobase.mli,v 5.6 2006-09-30 19:23:41 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Gwdb;

type name_index_data = array (array iper);
type strings_of_fsname = array (array istr);

value input : string -> base;

value check_magic : in_channel -> unit;
value magic_gwb : string;
value magic_gwb_iso_8859_1 : string;
value table_size : int;
value compare_istr : base -> istr -> istr -> int;
