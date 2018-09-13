(* $Id: mutil.mli,v 5.16 2007-02-24 16:16:57 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

val int_size : int
val verbose : bool ref

val list_iter_first : (bool -> 'a -> unit) -> 'a list -> unit
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

(** [array_to_list_map fn a] is almost like [Array.to_list a |> List.map fn]
    but it does not allocate an intermediate list.

    The list is constructed backward,
    so if [fn] have side effects it may not behave as excepted.
 *)
val array_to_list_map : ('a -> 'b) -> 'a array -> 'b list
