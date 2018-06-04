(* $Id: updateIndOk.mli,v 5.6 2008-01-21 13:28:12 ddr Exp $ *)

open Config
open Def
open Gwdb

val effective_del :
  config -> base -> (CheckItem.base_warning -> unit) -> person ->
    (iper, istr) gen_person
val effective_mod :
  config -> base -> (Update.key, string) gen_person -> (iper, istr) gen_person
val all_checks_person :
  config -> base -> (iper, istr) gen_person -> ifam gen_ascend ->
    ifam gen_union -> CheckItem.base_warning list
val print_mod_aux :
  config -> base -> ((Update.key, string) gen_person -> unit) -> unit

val rename_image_file :
  config -> base -> person -> (iper, string) gen_person -> unit

val print_add : config -> base -> unit
val print_del : config -> base -> unit
val print_mod : config -> base -> unit
val print_change_event_order : config -> base -> unit



(* Ajout pour l'API *)
val effective_add :
  config -> base -> (Update.key, string) Def.gen_person ->
    (iper, istr) Def.gen_person * 'a Def.gen_ascend
val raw_get : config -> string -> string
val strip_person :
  (string * 'a * 'b * 'c * 'd, string) Def.gen_person ->
    (string * 'a * 'b * 'c * 'd, string) Def.gen_person
val check_person :
  config -> base -> (string * string * 'b * 'c * 'd, string) Def.gen_person ->
    string option
val error_person : config -> base -> 'a -> string -> unit
val update_relations_of_related : base -> iper -> iper list -> unit
val reconstitute_death :
  config -> Def.date option -> Def.date option -> string -> Def.burial ->
    string -> Def.death
val reconstitute_from_pevents :
  ('a, string) Def.gen_pers_event list -> bool ->
    Def.codate * string * string * string ->
    Def.codate * string * string * string ->
    Def.death * string * string * string ->
    Def.burial * string * string * string ->
    (Def.codate * string * string * string) *
      (Def.codate * string * string * string) *
      (Def.death * string * string * string) *
      (Def.burial * string * string * string) *
      ('a, string) Def.gen_pers_event list
val check_conflict :
  config -> base -> ('a, string) Def.gen_person -> iper list -> unit
