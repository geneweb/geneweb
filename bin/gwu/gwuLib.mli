val out_dir : string ref
val old_gw : bool ref
val raw_output : bool ref
val separate_list : string list ref
val only_file : string ref
val sep_limit : int ref
val all_files : bool ref

val prepare_free_occ :
  ?select:(Geneweb_db.Driver.iper -> bool) -> Geneweb_db.Driver.base -> unit
(** Initializes the internal hashtables. Person whose identifier is not selected
    (`select p = false`) are ignored. *)

val gwu :
  Gwexport.gwexport_opts ->
  bool ->
  Geneweb_db.Driver.base ->
  string ->
  string ->
  (string, (string -> unit) * bool ref * (unit -> unit)) Hashtbl.t ->
  (Geneweb_db.Driver.iper -> bool) * (Geneweb_db.Driver.ifam -> bool) ->
  unit
(** Prints the `.gw` file. *)
