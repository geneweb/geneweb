(* $Id: secure.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value lang_path : unit -> list string;
value doc_path : unit -> list string;
value base_dir : unit -> string;
value add_lang_path : string -> unit;
value add_doc_path : string -> unit;
value set_base_dir : string -> unit;

value open_in : string -> in_channel;
value open_in_bin : string -> in_channel;
value open_out : string -> out_channel;
value open_out_bin : string -> out_channel;
value open_out_gen : list open_flag -> int -> string -> out_channel;
