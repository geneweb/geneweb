(* $Id: update.mli,v 2.6 1999-07-26 07:02:00 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

exception ModErr;
type key = (string * string * int);

value find_free_occ : base -> string -> string -> int -> int;
value infer_death : config -> option date -> death;
value print_same_name : config -> base -> person -> unit;

value link_person : config -> base -> (string * string * int) -> Adef.iper;
value insert_string : config -> base -> string -> Adef.istr;
value update_misc_names_of_family : base -> person -> unit;

value print_return : config -> unit;
value print_error : config -> base -> Gutil.base_error -> unit;
value print_warnings : config -> base -> list Gutil.base_warning -> unit;
value error : config -> base -> Gutil.base_error -> 'a;

value error_locked : config -> base -> unit;
value error_digest : config -> base -> 'a;

value digest_person : person -> Digest.t;
value digest_family : family -> Digest.t;

value reconstitute_date : config -> string -> option date;
value print_date :
  config -> base -> string -> string -> option date -> unit;

value print_src : config -> string -> string -> unit;

value print_someone : config -> base -> person -> unit;

value print : config -> base -> person -> unit;
