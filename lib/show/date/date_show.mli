type date = Geneweb_util.Date.date = Dgreg of dmy * calendar | Dtext of string

and calendar = Geneweb_util.Date.calendar =
  | Dgregorian
  | Djulian
  | Dfrench
  | Dhebrew

and dmy = Geneweb_util.Date.dmy = {
  day : int;
  month : int;
  year : int;
  prec : precision;
  delta : int;
}

and dmy2 = Geneweb_util.Date.dmy2 = {
  day2 : int;
  month2 : int;
  year2 : int;
  delta2 : int;
}

and precision = Geneweb_util.Date.precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

val pp_date : Format.formatter -> date -> unit
(** Printer for [date] *)

val show_date : date -> string
(** Convert [date] to string. *)

val pp_calendar : Format.formatter -> calendar -> unit
(** Printer for [calendar] *)

val show_calendar : calendar -> string
(** Convert [calendar] to string *)

val pp_dmy : Format.formatter -> dmy -> unit
(** Printer for [dmy] *)

val show_dmy : dmy -> string
(** Convert [dmy] to string *)

val pp_dmy2 : Format.formatter -> dmy2 -> unit
(** Printer for [dmy2] *)

val show_dmy2 : dmy2 -> string
(** Convert [dmy2] to string *)

val pp_precision : Format.formatter -> precision -> unit
(** Printer for [precision] *)

val show_precision : precision -> string
(** Convert [precision] to string *)
