(* $Id: checkItem.mli,v 1.2 2006-11-15 11:49:48 ddr Exp $ *)
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
    person -> option (list (Adef.iper * person));
value family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> Def.ifam ->
    family -> couple -> descend -> unit;
