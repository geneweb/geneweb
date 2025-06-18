module type S = sig
  type ctx
  (** Context of a hashing function. *)

  type 'a feeder = 'a -> ctx -> ctx
  (** Type of a feeder function for ['a]. *)

  type 'a hasher = ?salt:string -> 'a -> string
  (** Type of a hash function for ['a]. The salt [salt] is added to context
      before producing the final result. *)

  val feeder_to_hasher : 'a feeder -> 'a hasher
  (** [feeder_to_hasher f] converts the feeder [f] into a hash function. *)

  val ( <+> ) : (ctx -> ctx) -> (ctx -> ctx) -> ctx -> ctx
  (** [f <+> g] composes the two feeder functions. *)

  val string : string feeder
  val int : int feeder
  val bool : bool feeder
  val list : 'a feeder -> 'a list feeder
  val array : 'a feeder -> 'a array feeder
  val pair : 'a feeder -> 'b feeder -> ('a * 'b) feeder
  val option : 'a feeder -> 'a option feeder
  val iper : Geneweb_db.Driver.iper feeder
  val ifam : Geneweb_db.Driver.ifam feeder
  val istr : Geneweb_db.Driver.istr feeder
  val calendar : Adef.calendar feeder
  val dmy2 : Adef.dmy2 feeder
  val precision : Adef.precision feeder
  val dmy : Adef.dmy feeder
  val date : Adef.date feeder
  val cdate : Adef.cdate feeder
  val death_reason : Def.death_reason feeder
  val death : Def.death feeder
  val gen_title_name : 'string feeder -> 'string Def.gen_title_name feeder
  val gen_title : 'string feeder -> 'string Def.gen_title feeder
  val sex : Def.sex feeder
  val access : Def.access feeder
  val relation_type : Def.relation_type feeder
  val gen_couple : 'person feeder -> 'person Adef.gen_couple feeder
  val gen_union : 'family feeder -> 'family Def.gen_union feeder
  val gen_descend : 'person feeder -> 'person Def.gen_descend feeder

  val gen_relation :
    'person feeder ->
    'string feeder ->
    ('person, 'string) Def.gen_relation feeder

  val burial : Def.burial feeder

  val gen_pers_event_name :
    'string feeder -> 'string Def.gen_pers_event_name feeder

  val witness_kind : Def.witness_kind feeder

  val gen_pers_event :
    'person feeder ->
    'string feeder ->
    ('person, 'string) Def.gen_pers_event feeder

  val gen_person :
    'iper feeder ->
    'person feeder ->
    'string feeder ->
    ('iper, 'person, 'string) Def.gen_person feeder

  val divorce : Def.divorce feeder

  val gen_fam_event_name :
    'string feeder -> 'string Def.gen_fam_event_name feeder

  val gen_fam_event :
    'person feeder ->
    'string feeder ->
    ('person, 'string) Def.gen_fam_event feeder

  val relation_kind : Def.relation_kind feeder

  val gen_family :
    'person feeder ->
    'ifam feeder ->
    'string feeder ->
    ('person, 'ifam, 'string) Def.gen_family feeder
end

module Make (H : Digestif.S) : S with type ctx = H.ctx
module SHA256 : S with type ctx = Digestif.SHA256.ctx
