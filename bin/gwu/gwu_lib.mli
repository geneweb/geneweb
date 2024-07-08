val out_dir : string ref
val old_gw : bool ref
val raw_output : bool ref
val separate_list : string list ref
val only_file : string ref
val sep_limit : int ref

val prepare_free_occ : ?select:(Gwdb.iper -> bool) -> Gwdb.base -> unit
(** Initializes the internal hashtables. Person whose identifier is
    not selected (`select p = false`) are ignored. *)

val key_of_person :
  ?format_key:(surname:string -> firstname:string -> occ:int -> string) ->
  Gwdb.base ->
  Gwdb.person ->
  string option

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

val gwu_simple : export_isolated:bool -> Gwexport.gwexport_opts -> unit
(** same as [gwu] but with gwu executable parameter and setup *)
