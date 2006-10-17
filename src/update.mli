(* $Id: update.mli,v 5.5 2006-10-17 05:41:29 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Gwdb;

exception ModErr;
type create_info = (option date * string * death * option date * string);
type create = [ Create of sex and option create_info | Link ];
type key = (string * string * int * create * string);

value infer_death : config -> option date -> death;
value print_same_name : config -> base -> person -> unit;

value insert_person :
  config -> base -> string -> ref (list person) -> key -> Adef.iper
;
value add_misc_names_for_new_persons : config -> base -> list person -> unit;
value update_misc_names_of_family : config -> base -> person -> union -> unit;
value delete_topological_sort_v : config -> base -> unit;
value delete_topological_sort : config -> base -> unit;

value print_return : config -> unit;
value print_error : config -> base -> CheckItem.base_error -> unit;
value print_warnings : config -> base -> list CheckItem.base_warning -> unit;
value error : config -> base -> CheckItem.base_error -> 'a;

value error_locked : config -> unit;
value error_digest : config -> unit;

value digest_person : person -> Digest.t;
value digest_family :
  (gen_family key string * gen_couple key * gen_descend key) -> Digest.t;

value reconstitute_date : config -> string -> option date;

value print_someone : config -> base -> person -> unit;

value update_conf : config -> config;
