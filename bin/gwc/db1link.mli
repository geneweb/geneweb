val default_source : string ref

val do_check : bool ref

val do_consang : bool ref

val pr_stats : bool ref

val particules_file : string ref

type file_info = {
  mutable f_curr_src_file : string;
  mutable f_curr_gwo_file : string;
  mutable f_separate : bool;
  mutable f_bnotes : [ `drop | `erase | `first | `merge ];
  mutable f_shift : int;
  mutable f_local_names : (int * int, int) Hashtbl.t;
}

val link :
  (file_info -> unit -> Gwcomp.gw_syntax option) -> string -> bool
