(* $Id: srcfile.mli,v 4.0 2001-03-16 19:35:03 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print : config -> base -> string -> unit;
value print_source : config -> base -> string -> unit;
value print_start : config -> base -> unit;
value incr_welcome_counter : config -> option (int * int * string);
value incr_request_counter : config -> option (int * int * string);

value print_lexicon : config -> base -> unit;

value adm_file : string -> string;
