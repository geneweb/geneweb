val select_person :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.person -> Def.date option) ->
  bool ->
  (Geneweb_db.Driver.person * Def.dmy * Def.calendar) list * int
(** [select_person conf base get_date find_oldest] select 20 persons from the
    base according to the one of their date (birth, death, marriage, specific
    event, etc.) that could be get with [get_date]. Returns sorted by date
    persons that have the latest (if [find_oldest] is false) or oldest
    (otherwise) date. Selection could be different depending on environement
    [conf.env]. These variables affect the selection: k - allows to modify
    default value (20) of selected persons by,bm,bd - allows to set reference
    date (all dates after the reference one aren't selected) Returns also the
    number of selected persons *)

val select_family :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.family -> Def.date option) ->
  bool ->
  (Geneweb_db.Driver.family * Def.dmy * Def.calendar) list * int
(** Same as [select_person] but dealing with families *)

val death_date : Geneweb_db.Driver.person -> Adef.date option
(** Returns person's death date (if exists) *)

val make_population_pyramid :
  nb_intervals:int ->
  interval:int ->
  limit:int ->
  at_date:Def.dmy ->
  Config.config ->
  Geneweb_db.Driver.base ->
  int array * int array
(** [make_population_pyramid nb_intervals interval interval at_date conf base]
    Calculates population pyramid of all perons in the base. Population pyramid
    consists of two separated arrays that regroups number of men's and women's
    born in each time interval. One array has a size [nb_intervals + 1] and
    every element is a number of persons born in the giving time interval that
    represents [interval] years. Calculation starts at the date [at_date] and
    persons that are considered in pyramid should be alive at this date. [limit]
    allows to limit persons by age (those that has age greater then limit aren't
    taken into the account) *)
