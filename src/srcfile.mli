(* $Id: srcfile.mli,v 3.2 2000-06-03 21:08:06 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

value print : Config.config -> Def.base -> string -> unit;
value print_start : Config.config -> Def.base -> unit;
value incr_welcome_counter : Config.config -> option (int * int * string);
value incr_request_counter : Config.config -> option (int * int * string);

value print_lexicon : Config.config -> Def.base -> unit;

value adm_file : string -> string;
