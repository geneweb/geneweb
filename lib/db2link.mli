(* $Id: db2link.mli,v 5.2 2008-01-15 11:06:04 ddr Exp $ *)
(* Copyright (c) 2007-2008 INRIA *)

type file_info =
  { mutable f_curr_src_file : string;
    mutable f_curr_gwo_file : string;
    mutable f_separate : bool;
    mutable f_has_separates : bool;
    mutable f_sep_file_inx : int }

val particules_file : string ref
val do_check : bool ref
val do_consang : bool ref
val default_source : string ref
val pr_stats : bool ref

val link : (file_info -> unit -> Gwcomp.gw_syntax option) -> string -> bool
