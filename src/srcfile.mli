(* $Id: srcfile.mli,v 2.2 1999-03-23 21:15:47 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

value print : Config.config -> Def.base -> string -> unit;
value print_start : Config.config -> Def.base -> unit;
value incr_welcome_counter : Config.config -> option (int * int * string);
value incr_request_counter : Config.config -> option (int * int * string);
value hidden_env : Config.config -> unit;

value print_lexicon : Config.config -> Def.base -> unit;
