(* $Id: srcfile.mli,v 3.5 2000-11-08 21:36:53 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value print : config -> base -> string -> unit;
value print_source : config -> base -> string -> unit;
value print_start : config -> base -> unit;
value incr_welcome_counter : config -> option (int * int * string);
value incr_request_counter : config -> option (int * int * string);

value print_lexicon : config -> base -> unit;

value adm_file : string -> string;
