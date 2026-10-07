val print_differences :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper * Gwdb.iper) list ->
  Gwdb.ifam * Gwdb.family ->
  Gwdb.ifam * Gwdb.family ->
  unit
(** Displays differences between couples ; relation kind, marriage, marriage place
    and divorce. *)

val print :
  ?continue:
    (Config.config ->
    Gwdb.base ->
    (Update.key, Gwdb.ifam, string) Def.gen_family
    * Update.key Adef.gen_couple
    * Update.key Def.gen_descend ->
    string ->
    unit) ->
  Config.config ->
  Gwdb.base ->
  unit
(** Displays a menu for merging families. Couples must be identical (modulo reversion). *)
