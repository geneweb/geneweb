open Config
open Def
open Gwdb

val effective_del_no_commit : base -> (iper, iper, string) gen_person -> unit

val effective_del_commit : config -> base -> (iper, iper, string) gen_person -> unit

(** [effective_del] applies [effective_del_no_commit] and [effective_del_commit] *)
val effective_del : config -> base -> person -> unit

(** [effective_mod prerr ?skip_conflict conf base sp] *)
val effective_mod
  : ?prerr:(config -> base -> person error -> unit)
  -> ?skip_conflict:iper
  -> config
  -> base
  -> (iper, Update.key, string) gen_person
  -> (iper, iper, istr) gen_person

val print_mod
  : ?prerr:(config -> base -> person error -> unit) -> config -> base -> unit

val all_checks_person :
  base -> (iper, iper, istr) gen_person -> ifam gen_ascend ->
    ifam gen_union -> CheckItem.base_warning list

val print_mod_aux
  : config
  -> base
  -> string list ref
  -> string list ref
  -> ((iper, Update.key, string) gen_person -> unit)
  -> unit

val rename_image_file :
  config -> base -> person -> (iper, iper, string) gen_person -> unit

val print_add : config -> base -> unit
val print_del : config -> base -> unit

val print_change_event_order : config -> base -> unit

(* Ajout pour l'API *)
val effective_add
  : config -> base -> (iper, Update.key, string) Def.gen_person
  -> (iper, iper, istr) Def.gen_person * ifam Def.gen_ascend
val raw_get : config -> string -> string
val strip_person
  : (iper, string * 'a * 'b * 'c * 'd, string) Def.gen_person
  -> (iper, string * 'a * 'b * 'c * 'd, string) Def.gen_person
val check_person :
  config -> (iper, string * string * 'b * 'c * 'd, string) Def.gen_person ->
    string option
val error_person : config -> string -> unit
val update_relations_of_related : base -> iper -> iper list -> unit
val reconstitute_death :
  config -> Def.date option -> Def.date option -> string -> Def.burial ->
    string -> Def.death
val reconstitute_from_pevents :
  ('a, string) Def.gen_pers_event list -> bool ->
    Def.cdate * string * string * string ->
    Def.cdate * string * string * string ->
    Def.death * string * string * string ->
    Def.burial * string * string * string ->
    (Def.cdate * string * string * string) *
      (Def.cdate * string * string * string) *
      (Def.death * string * string * string) *
      (Def.burial * string * string * string) *
      ('a, string) Def.gen_pers_event list
