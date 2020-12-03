open Config
open Def
open Gwdb

val effective_del : config -> base -> person -> unit

val print_mod_ok
  : config
  -> base
  -> CheckItem.base_warning list
  -> (iper, ifam) Def.NLDB.page list
  -> (iper, iper, istr) Def.gen_person
  -> string
  -> string
  -> int
  -> string list
  -> string list
  -> unit

(** [effective_mod ?prerr ?prok ?skip_conflict conf base sp]

    On error, [prerr] is called.

    On success, [prok conf base wl pgl p ofn osn oocc deleted_relation deleted_string] is called.
*)
val effective_mod
  : ?prerr:(config -> base -> person error -> unit)
  -> ?skip_conflict:iper
  -> config
  -> base
  -> (iper, Update.key, string) gen_person
  -> (iper, iper, istr) gen_person

(** [print_mod ?prerr ?prok ?skip_conflict conf base sp]

    On error, [prerr] is called.

    On success, [prok conf base wl pgl p ofn osn oocc deleted_relation deleted_string] is called.
*)
val print_mod
  : ?prerr:(config -> base -> person error -> unit)
  -> ?prok:(config
            -> base
            -> CheckItem.base_warning list
            -> (iper, ifam) Def.NLDB.page list
            -> (iper, iper, istr) Def.gen_person
            -> string
            -> string
            -> int
            -> string list
            -> string list
            -> unit)
  -> config
  -> base
  -> unit

val all_checks_person :
  base -> (iper, iper, istr) gen_person -> ifam gen_ascend ->
    ifam gen_union -> CheckItem.base_warning list
val print_mod_aux :
  config -> base -> ((iper, Update.key, string) gen_person -> unit) -> unit

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
