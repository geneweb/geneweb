(* $Id: secure.mli,v 4.1 2002-12-31 08:38:07 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

value lang_path : ref (list string);
value doc_path : ref (list string);

value open_in : string -> in_channel;
value open_in_bin : string -> in_channel;
value open_out : string -> out_channel;
value open_out_bin : string -> out_channel;
value open_out_gen : list open_flag -> int -> string -> out_channel;
