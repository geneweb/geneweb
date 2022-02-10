(** Type that represents one update record stored in the history file for concerned person. *)
type gen_record = {
  date : string;
  wizard : string;
  gen_p : (Gwdb.iper, Gwdb.iper, string) Def.gen_person;
  gen_f : (Gwdb.iper, Gwdb.ifam, string) Def.gen_family list;
  gen_c : Gwdb.iper array list;
}

(** Returns history filename for the person with the given key.
    Has format : {i firstname.occ.surname} *)
val history_file : string -> string -> int -> string

(** Returns path to the history file inside {i history_d} with given filename *)
val history_path : Config.config -> string -> string

(** [record_diff conf base change] records new updated information [change]
    inside the history files of concerned by [change] persons. *)
val record_diff :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.ifam, string) Def.base_changed -> unit

(** Load list of modification records for a giving person's history file.
    The most recent modification is at the head of the list *)
val load_person_history : Config.config -> string -> gen_record list
