val out_dir : string ref
val old_gw : bool ref
val raw_output : bool ref
val separate_list : string list ref
val only_file : string ref
val sep_limit : int ref

val prepare_free_occ : ?select:(Gwdb.iper -> bool) -> Gwdb.base -> unit
(** Initializes the internal hashtables. Person whose identifier is
    not selected (`select p = false`) are ignored. *)

val gwu :
  Gwexport.gwexport_opts ->
  bool ->
  Gwdb.base ->
  string ->
  string ->
  (string, (string -> unit) * bool ref * (unit -> unit)) Hashtbl.t ->
  (Gwdb.iper -> bool) * (Gwdb.ifam -> bool) ->
  unit
(** Prints the `.gw` file. *)
