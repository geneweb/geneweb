(* $Id: srcfile.mli,v 3.1 2000-01-10 02:14:41 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

value print : Config.config -> Def.base -> string -> unit;
value print_start : Config.config -> Def.base -> unit;
value incr_welcome_counter : Config.config -> option (int * int * string);
value incr_request_counter : Config.config -> option (int * int * string);
value hidden_env : Config.config -> unit;

value print_lexicon : Config.config -> Def.base -> unit;

value adm_file : string -> string;
