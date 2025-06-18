type gen_record = {
  date : Adef.safe_string;
  wizard : Adef.safe_string;
  gen_p :
    (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) Def.gen_person;
  gen_f :
    (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam, string) Def.gen_family list;
  gen_c : Geneweb_db.Driver.iper array list;
}
(** Type that represents one update record stored in the history file for
    concerned person. *)

val history_file : string -> string -> int -> string
(** Returns history filename for the person with the given key. Has format :
    {i firstname.occ.surname} *)

val history_path : Config.config -> string -> string
(** Returns path to the history file inside {i history_d} with given filename *)

val record_diff :
  Config.config ->
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.ifam,
    string )
  Def.base_changed ->
  unit
(** [record_diff conf base change] records new updated information [change]
    inside the history files of concerned by [change] persons. *)

val load_person_history : Config.config -> string -> gen_record list
(** Load list of modification records for a giving person's history file. The
    most recent modification is at the head of the list *)
