(* camlp5r *)
(* $Id: db2link.mli,v 5.2 2008-01-15 11:06:04 ddr Exp $ *)
(* Copyright (c) 2007-2008 INRIA *)

type file_info =
  { f_curr_src_file : mutable string;
    f_curr_gwo_file : mutable string;
    f_separate : mutable bool;
    f_has_separates : mutable bool;
    f_sep_file_inx : mutable int }
;

value particules_file : ref string;
value do_check : ref bool;
value do_consang : ref bool;
value default_source : ref string;
value pr_stats : ref bool;

value link : (file_info -> unit -> option Gwcomp.gw_syntax) -> string -> bool;
