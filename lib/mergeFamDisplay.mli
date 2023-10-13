val print_differences :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper * Gwdb.iper) list ->
  Gwdb.ifam * Gwdb.family ->
  Gwdb.ifam * Gwdb.family ->
  unit
(** Displays differences between couples ; relation kind, marriage, marriage place
    and divorce. *)

val print : Config.config -> Gwdb.base -> unit
(** Displays a menu for merging families. Couples must be identical (modulo reversion). *)
