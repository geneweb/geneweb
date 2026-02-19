module Personal_event : sig
  type t

  val to_pers_event : t -> Gwdb.pers_event
end

module Family_event : sig
  type t

  val to_fam_event : t -> Gwdb.fam_event
end

module rec Person : sig
  type t

  val make : conf:Config.config -> base:Gwdb.base -> Gwdb.iper -> t
  val get_iper : t -> Gwdb.iper option
  val get_sex : t -> Def.sex option
  val get_first_name : t -> Gwdb.istr option
  val get_surname : t -> Gwdb.istr option
  val get_first_names_aliases : t -> Gwdb.istr list option
  val get_surnames_aliases : t -> Gwdb.istr list option
  val get_baptism : t -> Adef.cdate option
  val get_baptism_place : t -> Gwdb.istr option
  val get_birth : t -> Adef.cdate option
  val get_birth_place : t -> Gwdb.istr option
  val get_death : t -> Def.death option
  val get_death_place : t -> Gwdb.istr option
  val get_burial : t -> Def.burial option
  val get_burial_place : t -> Gwdb.istr option
  val get_occupation : t -> Gwdb.istr option

  val get_family :
    conf:Config.config -> base:Gwdb.base -> t -> Family.t array option

  val get_pevents : t -> Personal_event.t list option
end

and Family : sig
  type t

  val get_marriage : t -> Adef.cdate option
  val get_marriage_place : t -> Gwdb.istr option

  val get_spouse :
    conf:Config.config ->
    base:Gwdb.base ->
    person:Person.t ->
    t ->
    Person.t option

  val get_fevents : t -> Family_event.t list option
end
