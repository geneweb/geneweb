(* $Id: secure.mli,v 4.2 2003-01-05 17:42:18 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

value lang_path : unit -> list string;
value doc_path : unit -> list string;
value add_lang_path : string -> unit;
value add_doc_path : string -> unit;

value open_in : string -> in_channel;
value open_in_bin : string -> in_channel;
value open_out : string -> out_channel;
value open_out_bin : string -> out_channel;
value open_out_gen : list open_flag -> int -> string -> out_channel;
