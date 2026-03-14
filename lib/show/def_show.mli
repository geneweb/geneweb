val show_calendar : Adef.calendar -> string
(** Convert [calendar] to string *)

val pp_gen_title :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string Def.gen_title ->
  unit
(** Printer for [gen_title] *)

val pp_gen_relation :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'string) Def.gen_relation ->
  unit
(** Printer for [gen_relation] *)

val pp_sex : Format.formatter -> Def.sex -> unit
(** Printer for [sex] *)

val pp_access : Format.formatter -> Def.access -> unit
(** Printer for [access] *)

val pp_cdate : Format.formatter -> Adef.cdate -> unit
(** Printer for [cdate] *)

val pp_death : Format.formatter -> Def.death -> unit
(** Printer for [death] *)

val pp_burial : Format.formatter -> Def.burial -> unit
(** Printer for [burial] *)

val pp_gen_pers_event_name :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string Def.gen_pers_event_name ->
  unit
(** Printer for [gen_pers_event_name] *)

val show_relation_kind : Def.relation_kind -> string
(** Convert [relation_kind] to string *)

val show_gen_pers_event_name :
  (Format.formatter -> 'string -> unit) ->
  'string Def.gen_pers_event_name ->
  string
(** Convert [gen_pers_event_name] to string *)

val show_witness_kind : Def.witness_kind -> string
(** Convert [witness_kind] to string *)

val show_gen_fam_event_name :
  (Format.formatter -> 'string -> unit) ->
  'string Def.gen_fam_event_name ->
  string
(** Convert [gen_fam_event_name] to string *)

val pp_gen_pers_event :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'string) Def.gen_pers_event ->
  unit
(** Printer for [gen_pers_event] *)

val pp_relation_kind : Format.formatter -> Def.relation_kind -> unit
(** Printer for [relation_kind] *)

val show_relation_type : Def.relation_type -> string
(** Convert [relation_type] to string *)

val pp_fix : Format.formatter -> Adef.fix -> unit
(** Printer for [fix] *)

val show_fix : Adef.fix -> string
(** Convert [fix] to string *)

val pp_divorce : Format.formatter -> Def.divorce -> unit
(** Printer for [divorce] *)

val pp_gen_fam_event_name :
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  'string Def.gen_fam_event_name ->
  unit
(** Printer for [gen_fam_event_name] *)

val pp_gen_fam_event :
  (Format.formatter -> 'person -> unit) ->
  (Format.formatter -> 'string -> unit) ->
  Format.formatter ->
  ('person, 'string) Def.gen_fam_event ->
  unit
(** Printer for [gen_fam_event] *)
