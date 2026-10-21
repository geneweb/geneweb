val print_differences :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper) list ->
  Geneweb_db.Driver.ifam * Geneweb_db.Driver.family ->
  Geneweb_db.Driver.ifam * Geneweb_db.Driver.family ->
  unit
(** Displays differences between couples ; relation kind, marriage, marriage
    place and divorce. *)

val print : Config.config -> Geneweb_db.Driver.base -> unit
(** Displays a menu for merging families. Couples must be identical (modulo
    reversion). *)
