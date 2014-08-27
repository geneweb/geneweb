(* $Id: updateIndOk.mli,v 5.6 2008-01-21 13:28:12 ddr Exp $ *)

open Config;
open Def;
open Gwdb;

value effective_del :
  config -> base -> (CheckItem.base_warning -> unit) -> person ->
    gen_person iper istr;
value effective_mod :
  config -> base -> gen_person Update.key string -> gen_person iper istr;
value all_checks_person :
  config -> base -> gen_person iper istr -> gen_ascend ifam ->
    gen_union ifam -> list CheckItem.base_warning;
value print_mod_aux :
  config -> base -> (gen_person Update.key string -> unit) -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;



(* Ajout pour l'API *)
value effective_add :
  config -> base -> Def.gen_person Update.key string ->
    (Def.gen_person iper istr * Def.gen_ascend 'a);
value raw_get : config -> string -> string;
value strip_person :
  Def.gen_person (string * 'a * 'b * 'c * 'd) string ->
    Def.gen_person (string * 'a * 'b * 'c * 'd) string;
value check_person :
  config -> base -> Def.gen_person (string * string * 'b * 'c * 'd) string -> option string;
value error_person : config -> base -> 'a -> string -> unit;
value update_relations_of_related : base -> iper -> list iper -> unit;
value reconstitute_death :
  config -> option Def.date -> option Def.date -> string -> Def.burial -> string -> Def.death;
value reconstitute_from_pevents :
  list (Def.gen_pers_event 'a string) -> bool ->
  (Def.codate * string * string * string) ->
  (Def.codate * string * string * string) ->
  (Def.death * string * string * string) ->
  (Def.burial * string * string * string) ->
  ((Def.codate * string * string * string) *
  (Def.codate * string * string * string) *
  (Def.death * string * string * string) *
  (Def.burial * string * string * string) *
  list (Def.gen_pers_event 'a string));
value check_conflict :
  config -> base -> Def.gen_person 'a string -> list iper -> unit;
