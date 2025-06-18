(** Json converter driver *)
module type ConverterDriver = sig
  type t
  (** Json value *)

  val str : string -> t
  (** Convert to JSON string *)

  val int : int -> t
  (** Convert to JSON integer *)

  val obj : (string * t) array -> t
  (** Convert to JSON object *)

  val null : t
  (** Convert to JSON null value *)

  val array : 't array -> t
  (** Convert array to JSON list *)

  val list : 't list -> t
  (** Convert list to JSON list *)

  val bool : bool -> t
  (** Convert to JSON boolean *)
end

(** Functor building JSON convertion functions of the Geneweb data types. *)
module Make : functor (D : ConverterDriver) -> sig
  val conv_dmy : Def.dmy -> D.t
  (** Convert [dmy] to JSON *)

  val conv_dmy2 : Def.dmy2 -> D.t
  (** Convert [dmy2] to JSON *)

  val conv_cdate : Def.cdate -> D.t
  (** Convert [cdate] to JSON *)

  val conv_pevent_name : string Def.gen_pers_event_name -> D.t
  (** Convert [gen_pers_event_name] to JSON *)

  val conv_event_witness_kind : Def.witness_kind -> D.t
  (** Convert [witness_kind] to JSON *)

  val conv_pevent : (Geneweb_db.Driver.iper, string) Def.gen_pers_event -> D.t
  (** Convert [gen_pers_event] to JSON *)

  val conv_title_name : string Def.gen_title_name -> D.t
  (** Convert [gen_title_name] to JSON *)

  val conv_title : string Def.gen_title -> D.t
  (** Convert [gen_title] to JSON *)

  val conv_relation_kind : Def.relation_kind -> D.t
  (** Convert [relation_kind] to JSON *)

  val conv_fevent_name : string Def.gen_fam_event_name -> D.t
  (** Convert [gen_fam_event_name] to JSON *)

  val conv_fevent : (Geneweb_db.Driver.iper, string) Def.gen_fam_event -> D.t
  (** Convert [gen_fam_event] to JSON *)

  val conv_divorce : Def.divorce -> D.t
  (** Convert [divorce] to JSON *)

  val conv_relation_type : Def.relation_type -> D.t
  (** Convert [relation_type] to JSON *)

  val conv_rparent : (Geneweb_db.Driver.iper, string) Def.gen_relation -> D.t
  (** Convert [gen_relation] to JSON *)

  val conv_death : Def.death -> D.t
  (** Convert [death] to JSON *)

  val conv_person : Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> D.t
  (** Convert [person] to JSON *)

  val conv_family : Geneweb_db.Driver.base -> Geneweb_db.Driver.family -> D.t
  (** Convert [family] to JSON *)
end
