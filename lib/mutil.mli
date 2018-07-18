(* $Id: mutil.mli,v 5.16 2007-02-24 16:16:57 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

val int_size : int
val verbose : bool ref
val utf_8_db : bool ref

val lindex : string -> char -> int option
val rindex : string -> char -> int option
val array_mem : 'a -> 'a array -> bool
val list_iter_first : (bool -> 'a -> unit) -> 'a list -> unit
val list_uniq : 'a list -> 'a list
val tr : char -> char -> string -> string
val strip_all_trailing_spaces : string -> string

val decline : char -> string -> string
val nominative : string -> string

val remove_file : string -> unit
val mkdir_p : string -> unit
val remove_dir : string -> unit
val lock_file : string -> string

val output_value_no_sharing : out_channel -> _ -> unit
val output_array_no_sharing : out_channel -> (int -> _) -> int -> unit

val name_key : string -> string
val initial : string -> int
val input_particles : string -> string list
val surnames_pieces : string -> string list

val utf_8_of_iso_8859_1 : string -> string
val iso_8859_1_of_utf_8 : string -> string

val roman_of_arabian : int -> string
val arabian_of_roman : string -> int

val start_with : string -> string -> bool

val compare_after_particle : string list -> string -> string -> int

val input_lexicon :
  string -> (string, string) Hashtbl.t -> (unit -> in_channel) -> unit

module StrSet : Set.S with type elt = string
