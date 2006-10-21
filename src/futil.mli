(* $Id: futil.mli,v 5.2 2006-10-21 09:13:13 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Def;

value map_title_strings : ('a -> 'b) -> gen_title 'a -> gen_title 'b;
value map_relation_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_relation 'a 'b -> gen_relation 'c 'd
;
value map_person_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_person 'a 'b -> gen_person 'c 'd
;
value map_family_ps :
  ('a -> 'c) -> ('b -> 'd) -> gen_family 'a 'b -> gen_family 'c 'd
;
value map_couple_p : bool -> ('a -> 'b) -> gen_couple 'a -> gen_couple 'b;
value map_descend_p : ('a -> 'b) -> gen_descend 'a -> gen_descend 'b;

value parent : bool -> array 'a -> gen_couple 'a;

value gen_person_misc_names :
  string -> string -> string -> list string -> list string -> list string ->
    list string -> list (Def.gen_title string) ->
    list (string * list string) -> list string -> list string;
