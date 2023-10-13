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

type file_info = {
  mutable f_curr_src_file : string;
  mutable f_curr_gwo_file : string;
  mutable f_separate : bool;
  mutable f_bnotes : [ `drop | `erase | `first | `merge ];
  mutable f_shift : int;
  mutable f_local_names : (int * int, int) Hashtbl.t;
}
(** Information about current .gwo file. *)

val link : (file_info -> unit -> Gwcomp.gw_syntax option) -> string -> bool
(** Link .gwo files and create a database. *)
