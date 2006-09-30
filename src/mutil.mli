(* $Id: mutil.mli,v 5.1 2006-09-30 18:07:33 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

value int_size : int;
value verbose : ref bool;
value utf_8_db : ref bool;

value rindex : string -> char -> option int;
value array_memq : 'a -> array 'a -> bool;

value decline : char -> string -> string;
value nominative : string -> string;

value remove_file : string -> unit;
value mkdir_p : string -> unit;
value remove_dir : string -> unit;

value output_value_no_sharing : out_channel -> 'a -> unit;

value initial : string -> int;
value input_particles : string -> list string;
value surnames_pieces : string -> list string;
