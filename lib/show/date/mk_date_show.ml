type date = Geneweb_util.Date.date = Dgreg of dmy * calendar | Dtext of string
[@@deriving show]

and calendar = Geneweb_util.Date.calendar =
  | Dgregorian
  | Djulian
  | Dfrench
  | Dhebrew
[@@deriving show]

and dmy = Geneweb_util.Date.dmy = {
  day : int;
  month : int;
  year : int;
  prec : precision;
  delta : int;
}
[@@deriving show]

and dmy2 = Geneweb_util.Date.dmy2 = {
  day2 : int;
  month2 : int;
  year2 : int;
  delta2 : int;
}
[@@deriving show]

and precision = Geneweb_util.Date.precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2
[@@deriving show]
