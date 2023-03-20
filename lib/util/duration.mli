type display = { nb_day : int; nb_month : int; nb_year : int }
type precision = Less | Exact | More | Undefined
type t = private { sdn : int; prec : precision; display : display }

val of_sdn : prec:precision -> int -> t
val compare : t -> t -> int

val time_elapsed : Date.dmy -> Date.dmy -> t
(** [time_elapsed start stop]
    Compute the time elapsed between [start] and [stop].
    If [stop] is prior to [start], resulting [dmy]'s field
    are negative (but correct).
    Used to compare only gregorian calendar's dates.
 *)

val time_elapsed_opt : Date.dmy -> Date.dmy -> t option
(** Same as [time_elapsed], but will return [None]
    if computation is not possible
    (e.g. time_elapsed_opt /1839 /1859). *)

val add : t -> t -> t
val of_years : int -> t
val of_months : int -> t
val of_days : int -> t
