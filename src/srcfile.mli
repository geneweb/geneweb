(* $Id: srcfile.mli,v 1.1.1.1 1998-09-01 14:32:06 ddr Exp $ *)

value print : Config.config -> Def.base -> string -> unit;
value print_start : Config.config -> Def.base -> unit;
value incr_welcome_counter : Config.config -> unit;
value incr_request_counter : Config.config -> unit;
value hidden_env : Config.config -> unit;

value print_lexicon : Config.config -> Def.base -> unit;
