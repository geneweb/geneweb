(* $Id: srcfile.mli,v 2.3 1999-08-06 02:22:34 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

value print : Config.config -> Def.base -> string -> unit;
value print_start : Config.config -> Def.base -> unit;
value incr_welcome_counter : Config.config -> option (int * int * string);
value incr_request_counter : Config.config -> option (int * int * string);
value hidden_env : Config.config -> unit;

value print_lexicon : Config.config -> Def.base -> unit;

value adm_file : string -> string;
