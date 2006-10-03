(* $Id: dutil.mli,v 5.1 2006-10-03 03:42:33 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Config;
open Dbdisk;

value dsk_person_misc_names :
  dsk_base -> dsk_person -> (dsk_person -> list dsk_title) -> list string;

value dsk_nobtit : config -> dsk_base -> dsk_person -> list dsk_title;
