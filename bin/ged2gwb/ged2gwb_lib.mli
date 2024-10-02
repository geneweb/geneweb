type month_number_dates =
  | MonthDayDates
  | DayMonthDates
  | NoMonthNumberDates
  | MonthNumberHappened of string

type charset = Ansel | Ansi | Ascii | Msdos | MacIntosh | Utf8
type case = NoCase | LowerCase | UpperCase

module State : sig
  val log_oc : out_channel ref
  val lowercase_first_names : bool ref
  val track_ged2gw_id : bool ref
  val case_surnames : case ref
  val extract_first_names : bool ref
  val extract_public_names : bool ref
  val charset_option : charset option ref
  val alive_years : int ref
  val dead_years : int ref
  val try_negative_dates : bool ref
  val no_negative_dates : bool ref
  val no_public_if_titles : bool ref
  val first_names_brackets : (char * char) option ref
  val untreated_in_notes : bool ref
  val force : bool ref
  val default_source : string ref
  val relation_status : Def.relation_kind ref
  val no_picture : bool ref
  val do_check : bool ref
  val particles : string list ref
  val in_file : string ref
  val out_file : string ref
  val month_number_dates : month_number_dates ref

  type t

  val make : unit -> t
end

val warning_month_number_dates : unit -> unit
val make_base : State.t -> Gwdb.base
