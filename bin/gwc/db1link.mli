(** Default source field for persons and families without source data *)
val default_source : string ref

(** Base consistency check *)
val do_check : bool ref

(** Compute consanguinity *)
val do_consang : bool ref

(** Print base's statistics *)
val pr_stats : bool ref

(** File containing the particles to use *)
val particules_file : string ref

(** Information about current .gwo file. *)
type file_info = {
  mutable f_curr_src_file : string;
  mutable f_curr_gwo_file : string;
  mutable f_separate : bool;
  mutable f_bnotes : [ `drop | `erase | `first | `merge ];
  mutable f_shift : int;
  mutable f_local_names : (int * int, int) Hashtbl.t;
}

(** Link .gwo files and create a database. *)
val link :
  (file_info -> unit -> Gwcomp.gw_syntax option) -> string -> bool
