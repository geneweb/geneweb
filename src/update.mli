(* $Id: update.mli,v 5.12 2008-01-08 02:08:00 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;

exception ModErr;
type create_info =
  { ci_birth_date : option date;
    ci_birth_place : string;
    ci_death : death;
    ci_death_date : option date;
    ci_death_place : string;
    ci_occupation : string;
    ci_public : bool }
;
type create = [ Create of sex and option create_info | Link ];
type key = (string * string * int * create * string);

value infer_death : config -> option date -> death;
value print_same_name : config -> base -> person -> unit;

value insert_person :
  config -> base -> string -> ref (list (gen_person iper istr)) -> key ->
    Adef.iper
;
value add_misc_names_for_new_persons :
  base -> list (gen_person iper istr) -> unit
;
value update_misc_names_of_family : base -> sex -> gen_union ifam -> unit;
value delete_topological_sort_v : config -> base -> unit;
value delete_topological_sort : config -> base -> unit;

value update_related_pointers :
  base -> iper -> list iper -> list iper -> unit;

value print_return : config -> unit;
value print_error : config -> base -> CheckItem.base_error -> unit;
value print_warnings : config -> base -> list CheckItem.base_warning -> unit;
value print_miscs : config -> base -> list CheckItem.base_misc -> unit;
value print_warnings_and_miscs :
  config -> base -> 
    (list CheckItem.base_warning * list CheckItem.base_misc) -> unit;
value error : config -> base -> CheckItem.base_error -> 'a;

value error_locked : config -> unit;
value error_digest : config -> unit;

value digest_person : gen_person key string -> Digest.t;
value digest_family :
  (gen_family key string * gen_couple key * gen_descend key) -> Digest.t;

value reconstitute_date : config -> string -> option date;

value print_someone : config -> base -> person -> unit;

value update_conf : config -> config;
