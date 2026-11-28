exception Critical_import_error of string

val default_source : string ref
(** Default source field for persons and families without source data *)

val do_check : bool ref
(** Base consistency check *)

val do_consang : bool ref
(** Compute consanguinity *)

val pr_stats : bool ref
(** Print base's statistics *)

val particules_file : string ref
(** File containing the particles to use *)

type bnotes = Merge | Erase | First | Drop

type file_info = {
  mutable f_curr_src_file : string;
  mutable f_curr_gwo_file : string;
  mutable f_separate : bool;
  mutable f_bnotes : bnotes;
  mutable f_shift : int;
  mutable f_local_names : (int * int, int) Hashtbl.t;
}
(** Information about current .gwo file. *)

val folding_collision : bool ref
(** Set by [link] when two persons get the same key with the current folding of
    the names, and different keys with the former one ([Unidecode.former]). *)

val duplicates : (string * string * int * int) list ref
(** Set by [link]: first name, surname, occurrence number in the [.gw] and
    occurrence number given of each person defined with the key of another one
    (not explained by the former folding of the names), renumbered with the
    first free occurrence number. *)

val link :
  ?no_warn:bool ->
  (file_info -> unit -> Gwcomp.gw_syntax option) ->
  string ->
  bool
(** Link .gwo files and create a database. *)
