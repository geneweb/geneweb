(* $Id: srcfile.mli,v 2.1 1999-03-08 11:19:15 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

value print : Config.config -> Def.base -> string -> unit;
value print_start : Config.config -> Def.base -> unit;
value incr_welcome_counter : Config.config -> unit;
value incr_request_counter : Config.config -> unit;
value hidden_env : Config.config -> unit;

value print_lexicon : Config.config -> Def.base -> unit;
