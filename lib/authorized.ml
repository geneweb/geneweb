module Personal_event : sig
  type t

  val make : Gwdb.pers_event -> t
  val to_pers_event : t -> Gwdb.pers_event
end = struct
  type t = Gwdb.pers_event

  let make event = event
  let to_pers_event event = event
end

module Family_event : sig
  type t

  val make : Gwdb.fam_event -> t
  val to_fam_event : t -> Gwdb.fam_event
end = struct
  type t = Gwdb.fam_event

  let make event = event
  let to_fam_event event = event
end

module Person' = Person

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
end = struct
  type authorization_level =
    | Navigation_without_names
    | Navigation_with_names
    | Full

  let navigation_with_names_authorized = function
    | Navigation_without_names -> false
    | Navigation_with_names | Full -> true

  let fully_authorized = function
    | Navigation_without_names | Navigation_with_names -> false
    | Full -> true

  type t' = { person : Gwdb.person; authorization_level : authorization_level }
  type t = t' option

  let make ~conf ~base person_id =
    let person = Gwdb.poi base person_id in
    if Person'.is_visible conf base person then
      Some { person; authorization_level = Full }
    else if Person'.has_visible_name conf base person then
      Some { person; authorization_level = Navigation_with_names }
    else if Person'.is_restricted conf base (Gwdb.get_iper person) then None
    else Some { person; authorization_level = Navigation_without_names }

  let get_if_navigation_authorized ~get person =
    Option.map (fun person -> get person.person) person

  let get_if_navigation_with_names_authorized ~get person =
    let ( >>= ) = Option.bind in
    person >>= fun person ->
    Ext_option.return_if
      (navigation_with_names_authorized person.authorization_level) (fun () ->
        get person.person)

  let get_if_fully_authorized ~get person =
    let ( >>= ) = Option.bind in
    person >>= fun person ->
    Ext_option.return_if (fully_authorized person.authorization_level)
      (fun () -> get person.person)

  let get_iper person = get_if_navigation_authorized ~get:Gwdb.get_iper person
  let get_sex person = get_if_navigation_authorized ~get:Gwdb.get_sex person

  let get_first_name person =
    get_if_navigation_with_names_authorized ~get:Gwdb.get_first_name person

  let get_surname person =
    get_if_navigation_with_names_authorized ~get:Gwdb.get_surname person

  let get_first_names_aliases person =
    get_if_navigation_with_names_authorized ~get:Gwdb.get_first_names_aliases
      person

  let get_surnames_aliases person =
    get_if_navigation_with_names_authorized ~get:Gwdb.get_surnames_aliases
      person

  let get_baptism person = get_if_fully_authorized ~get:Gwdb.get_baptism person

  let get_baptism_place person =
    get_if_fully_authorized ~get:Gwdb.get_baptism_place person

  let get_birth person = get_if_fully_authorized ~get:Gwdb.get_birth person

  let get_birth_place person =
    get_if_fully_authorized ~get:Gwdb.get_birth_place person

  let get_death person = get_if_fully_authorized ~get:Gwdb.get_death person

  let get_death_place person =
    get_if_fully_authorized ~get:Gwdb.get_death_place person

  let get_burial person = get_if_fully_authorized ~get:Gwdb.get_burial person

  let get_burial_place person =
    get_if_fully_authorized ~get:Gwdb.get_burial_place person

  let get_occupation person =
    get_if_fully_authorized ~get:Gwdb.get_occupation person

  let get_family ~conf ~base person =
    get_if_navigation_authorized
      ~get:(fun person ->
        person |> Gwdb.get_family |> Array.map (Family.make ~conf ~base))
      person

  let get_pevents person =
    get_if_fully_authorized
      ~get:(fun person ->
        person |> Gwdb.get_pevents |> List.map Personal_event.make)
      person
end

and Family : sig
  type t

  val make : conf:Config.config -> base:Gwdb.base -> Gwdb.ifam -> t
  val get_marriage : t -> Adef.cdate option
  val get_marriage_place : t -> Gwdb.istr option

  val get_spouse :
    conf:Config.config ->
    base:Gwdb.base ->
    person:Person.t ->
    t ->
    Person.t option

  val get_fevents : t -> Family_event.t list option
end = struct
  type authorization_level = Navigation | Full

  let fully_authorized = function Navigation -> false | Full -> true

  type t = { family : Gwdb.family; authorization_level : authorization_level }

  let make ~conf ~base family_id =
    let family = Gwdb.foi base family_id in
    let is_visible =
      Array.for_all
        (fun person_id ->
          Person'.is_visible conf base (Gwdb.poi base person_id))
        (Gwdb.get_parent_array family)
    in
    { family; authorization_level = (if is_visible then Full else Navigation) }

  let get_if_fully_authorized ~get family =
    Ext_option.return_if (fully_authorized family.authorization_level)
      (fun () -> get family.family)

  let get_marriage family =
    get_if_fully_authorized ~get:Gwdb.get_marriage family

  let get_marriage_place family =
    get_if_fully_authorized ~get:Gwdb.get_marriage_place family

  let get_father ~conf ~base family =
    family.family |> Gwdb.get_father |> Person.make ~conf ~base

  let get_mother ~conf ~base family =
    family.family |> Gwdb.get_mother |> Person.make ~conf ~base

  let get_spouse ~conf ~base ~person family =
    match Person.get_iper person with
    | None -> None
    | Some person_id -> (
        match
          ( family |> get_father ~conf ~base |> Person.get_iper,
            family |> get_mother ~conf ~base |> Person.get_iper )
        with
        | None, None | None, Some _ | Some _, None -> None
        | Some father_id, Some mother_id ->
            if Gwdb.eq_iper person_id father_id then
              Some (Person.make ~conf ~base mother_id)
            else if Gwdb.eq_iper person_id mother_id then
              Some (Person.make ~conf ~base father_id)
            else None)

  let get_fevents family =
    get_if_fully_authorized
      ~get:(fun family ->
        family |> Gwdb.get_fevents |> List.map Family_event.make)
      family
end
