(* $Id: mutil.mli,v 5.5 2006-10-01 14:31:08 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

value int_size : int;
value verbose : ref bool;
value utf_8_db : ref bool;

value rindex : string -> char -> option int;
value array_memq : 'a -> array 'a -> bool;
value tr : char -> char -> string -> string;
value strip_all_trailing_spaces : string -> string;

value decline : char -> string -> string;
value nominative : string -> string;

value remove_file : string -> unit;
value mkdir_p : string -> unit;
value remove_dir : string -> unit;
value lock_file : string -> string;

value output_value_no_sharing : out_channel -> 'a -> unit;

value name_key : string -> string;
value initial : string -> int;
value input_particles : string -> list string;
value surnames_pieces : string -> list string;

value utf_8_of_iso_8859_1 : string -> string;
value iso_8859_1_of_utf_8 : string -> string;
