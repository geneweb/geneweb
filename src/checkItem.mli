(* $Id: checkItem.mli,v 1.3 2006-12-25 22:56:03 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Gwdb;

type base_error = Def.error person;
type base_warning = Def.warning person descend title;

value leap_year : int -> bool;
value nb_days_in_month : int -> int -> int;
value time_elapsed : Def.dmy -> Def.dmy -> Def.dmy;
value strictly_before_dmy : Def.dmy -> Def.dmy -> bool;
value strictly_before : Def.date -> Def.date -> bool;
value strictly_after_dmy : Def.dmy -> Def.dmy -> bool;
value strictly_after : Def.date -> Def.date -> bool;
value date_of_death : Def.death -> option Adef.date;

value person :
  base -> (base_error -> unit) -> (base_warning -> unit) ->
    person -> option (list (Adef.iper * Def.gen_person Def.iper istr));
value family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> Def.ifam ->
    family -> couple -> descend -> unit;
