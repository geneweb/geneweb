type gen_record = {
  date : Adef.safe_string;
  wizard : Adef.safe_string;
  gen_p :
    (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) Def.gen_person;
  gen_f :
    (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam, string) Def.gen_family list;
  gen_c : Geneweb_db.Driver.iper array list;
  gen_s : Geneweb_db.Driver.iper list;
}
(** One update record of the history file of a person: the person, its families
    with their children ([gen_c]) and spouses ([gen_s]), in the same order. The
    files store persons by key, so that a record read back after a rebuild of
    the base designates the same persons: a person whose key is no longer found
    is left out of the lists, or is the dummy index in [gen_s], and a family is
    found back by its spouse (dummy [fam_index] otherwise). A person whose key
    is shared (["?"] names) is found back by its index only if this index still
    holds that key and, for a child, a child of the family. Records written
    before this format (GnHd0003) are read with their indexes as they were. *)

type entry
(** A record of a history file as stored, without reading its persons. *)

val entry_date : entry -> Adef.safe_string
val entry_wizard : entry -> Adef.safe_string

val load_entries : Config.config -> string -> entry list
(** [load_entries conf fname] Records of the history file [fname], the most
    recent first. *)

val write_entries : Config.config -> string -> entry list -> unit
(** [write_entries conf fname entries] Rewrites the history file [fname] with
    [entries], the oldest first, unchanged; removes it if [entries] is empty. *)

val history_file : string -> string -> int -> string
(** Returns history filename for the person with the given key. Has format :
    {i firstname.occ.surname} *)

val history_path : Config.config -> string -> string
(** Returns path to the history file inside {i history_d} with given filename *)

val archive_deleted : Config.config -> ('a, 'b, string) Def.gen_person -> unit
(** [archive_deleted conf p] moves the history file of the deleted person [p] to
    the folder {i _deleted} of {i history_d}, under the same subfolders,
    appending it to an archived file of the same key if any, so that a person
    later given that key does not inherit it. Files of persons whose first name
    or surname has no letter or digit (["?"], ["??"]) are shared by all such
    persons and stay in place. *)

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

val change_records :
  Config.config ->
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.ifam,
    string )
  Def.base_changed ->
  (gen_record * gen_record) option
(** [change_records conf base change] Records of the main person of [change]
    before and after it, built from the current state of the base. The children
    of a modified family, not carried by [change], keep their current value in
    the record before. [None] if [change] has no main person or leaves its
    record unchanged. *)

val record_of_entry :
  Geneweb_db.Driver.base -> entry list -> entry -> gen_record
(** [record_of_entry base entries e] The record [e] of the history file whose
    records are [entries], its persons read in [base]. The file follows the
    renames of its person: the person of a record stored by key is the one of
    the most recent record. *)
