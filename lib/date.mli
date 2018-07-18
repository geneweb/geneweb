(* $Id: date.mli,v 5.4 2007-03-14 00:39:57 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val dmy_of_dmy2 : dmy2 -> dmy
val code_dmy : config -> dmy -> string
val string_of_ondate : config -> date -> string
val string_of_ondate_aux : config -> date -> string
val string_of_date : config -> date -> string
val string_of_date_sep : config -> string -> date -> string
val string_slash_of_date : config -> date -> string
val string_of_age : config -> dmy -> string
val prec_year_text : config -> dmy -> string
val prec_text : config -> dmy -> string
val day_text : dmy -> string
val month_text : dmy -> string
val year_text : dmy -> string
val short_dates_text : config -> base -> person -> string
val short_marriage_date_text :
  config -> base -> family -> person -> person -> string
val print_dates : config -> base -> person -> unit
val print_calendar : config -> unit
val get_birth_death_date : person -> date option * date option * bool

val before_date : dmy -> dmy -> bool
  (* [before_date d1 d2] = True if d2 before d1; I know, it is not logical *)

(* return the day of the week given the date as parameter *)
val get_wday : config -> date -> string

val compare_date : date -> date -> int


(* Ajout pour l'API *)
val death_symbol : config -> string
val string_of_dmy : config -> dmy -> string
val string_of_on_french_dmy : config -> dmy -> string
val string_of_on_hebrew_dmy : config -> dmy -> string
val string_of_prec_dmy : config -> string -> string -> dmy -> string
val gregorian_precision : config -> dmy -> string
val french_month : config -> int -> string
val code_french_year : config -> int -> string
val code_hebrew_date : config -> int -> int -> int -> string
