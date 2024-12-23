val get : Config.config -> string -> string
val get_nth : Config.config -> string -> int -> string option
val getn : Config.config -> string -> string -> string
val get_purged_fn_sn : string list ref -> string -> string -> string * string
val getenv_sex : Config.config -> string -> Def.sex

val getn_p :
  Config.config ->
  string ->
  ?create_info:Update.create_info ->
  Def.sex ->
  Update.create

val reconstitute_sorted_events : Config.config -> int -> (int * int) list

val reconstitute_somebody :
  string list ref ->
  Config.config ->
  string ->
  string * string * int * Update.create * string

val sort_families_array_by_date : Gwdb.base -> Gwdb.ifam array -> unit
val bool_val : bool -> _ TemplAst.expr_val
val str_val : string -> _ TemplAst.expr_val
val safe_val : Adef.safe_string -> _ TemplAst.expr_val
val eval_default_var : Config.config -> string -> _ TemplAst.expr_val
val eval_date_var : Geneweb_util.Date.date option -> string -> _ TemplAst.expr_val
