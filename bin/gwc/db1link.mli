type file_info = {
  mutable f_curr_src_file : string;
  mutable f_curr_gwo_file : string;
  mutable f_separate : bool;
  mutable f_bnotes : [ `drop | `erase | `first | `merge ];
  mutable f_shift : int;
  mutable f_local_names : (int * int, int) Hashtbl.t;
}
(** Information about current .gwo file. *)

val link :
  save_mem:bool ->
  State.t ->
  (file_info -> unit -> Gwcomp.gw_syntax option) ->
  string ->
  bool
(** Link .gwo files and create a database. *)
