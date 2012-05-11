(* $Id: checkItem.mli,v 1.12 2007-09-05 13:19:25 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb;

type base_error = Def.error person;
type base_warning = Def.warning person family title;
type base_misc = Def.misc person family title;

value leap_year : int -> bool;
value nb_days_in_month : int -> int -> int;
value time_elapsed : Def.dmy -> Def.dmy -> Def.dmy;
value strictly_before_dmy : Def.dmy -> Def.dmy -> bool;
value strictly_before : Def.date -> Def.date -> bool;
value strictly_after_dmy : Def.dmy -> Def.dmy -> bool;
value strictly_after : Def.date -> Def.date -> bool;
value date_of_death : Def.death -> option Adef.date;

value person :
  base -> (base_warning -> unit) -> person ->
    option
      (list (Adef.iper * person * option Def.sex * option (list relation)));

value family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> Def.ifam ->
    family -> unit;

value reduce_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> Def.ifam ->
    family -> unit;

value sort_children :
  base -> array Adef.iper -> option (array Adef.iper * array Adef.iper);

value check_other_fields :
  base -> (base_misc -> unit) -> Def.ifam -> family -> unit;
