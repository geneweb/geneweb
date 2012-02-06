(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb;
open Config;

value string_of_titles : config -> base -> bool -> string -> person -> string;
value string_of_marriage_text : config -> base -> family -> string;
value interp_templ : string -> config -> base -> person -> unit;

value print : config -> base -> person -> unit;
value print_ascend : config -> base -> person -> unit;
value print_what_links : config -> base -> person -> unit;

value build_sosa_ht : config -> base -> unit; 
value get_sosa_person : config -> base -> person -> Num.t;
value get_single_sosa : config -> base -> person -> Num.t;

value string_of_num : string -> Num.t -> string;

(**)

value infinite : int;
value limit_desc : config -> int;
value make_desc_level_table :
  config -> base -> int -> person -> (array int * array int);
value default_max_cousin_lev : int;

type dup =
  [ DupFam of Adef.ifam and Adef.ifam
  | DupInd of Adef.iper and Adef.iper
  | NoDup ]
;

type excl_dup = (list (Adef.iper * Adef.iper) * list (Adef.ifam * Adef.ifam));

value excluded_possible_duplications : config -> excl_dup;
value first_possible_duplication : base -> Adef.iper -> excl_dup -> dup;
