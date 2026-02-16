(* TODO
   should it be is_empty_name instead? (deleted person have surname and first_name = "?")
   I don't think it is possible to have surname = empty_string *)
let is_empty p = Gwdb.is_empty_string (Gwdb.get_surname p)

let is_contemporary' conf base private_years p =
  let death = Gwdb.get_death p in
  if death = NotDead then private_years >= 0
  else
    let check_date d none =
      match d with
      | None -> none ()
      | Some d ->
          let a = Date.time_elapsed d conf.Config.today in
          if a.Date.year > private_years then false
          else if a.year < private_years then true
          else a.month = 0 && a.day = 0
    in
    check_date (Gwdb.get_birth p |> Date.cdate_to_dmy_opt) @@ fun () ->
    check_date (Gwdb.get_baptism p |> Date.cdate_to_dmy_opt) @@ fun () ->
    check_date (Gwdb.get_death p |> Date.dmy_of_death) @@ fun () ->
    let is_contemporary_marriage ifam =
      let marr_date_opt =
        Date.cdate_to_dmy_opt (Gwdb.get_marriage (Gwdb.foi base ifam))
      in
      check_date marr_date_opt (fun _ -> false)
    in
    let has_contemporary_marriage p =
      let families = Gwdb.get_family p in
      Array.exists is_contemporary_marriage families
    in
    (Gwdb.get_access p = Def.Private || not conf.public_if_no_date)
    || has_contemporary_marriage p

let is_contemporary conf base p =
  let private_years =
    if conf.Config.private_years < 1 then
      conf.default_contemporary_private_years
    else conf.private_years
  in
  is_empty p || is_contemporary' conf base private_years p

(** Calcul les droits de visualisation d'une personne en
      fonction de son age.
      Renvoie (dans l'ordre des tests) :
      - Vrai si : magicien ou ami ou la personne est public
      - Vrai si : la personne est en si_titre, si elle a au moins un
                  titre et que public_if_title = yes dans le fichier gwf
      - Faux si : la personne n'est pas décédée et private_years > 0
      - Vrai si : la personne est plus agée (en fonction de la date de
                  naissance ou de la date de baptème) que privates_years
      - Faux si : la personne est plus jeune (en fonction de la date de
                  naissance ou de la date de baptème) que privates_years
      - Vrai si : la personne est décédée depuis plus de privates_years
      - Faux si : la personne est décédée depuis moins de privates_years
      - Vrai si : la personne a entre 80 et 120 ans et qu'elle n'est pas
                  privée et public_if_no_date = yes
      - Vrai si : la personne s'est mariée depuis plus de private_years
      - Faux dans tous les autres cas *)
let is_visible conf base p =
  (not (is_empty p))
  && (conf.Config.wizard || conf.friend
     || Gwdb.get_access p = Public
     || conf.public_if_titles
        && Gwdb.get_access p = IfTitles
        && Gwdb.nobtitles base conf.allowed_titles conf.denied_titles p <> []
     || not (is_contemporary' conf base conf.Config.private_years p))

(* aswer the question "should we show p's names and other private stuffs?";
   takes into account if you are a wizard or not *)
(* TODO ??
   - change conf.hide_names to not take in account wizard|friend;
   - take into account wizard|friend in this function
   - and authorized_age
   - rename to is_hidden
   ??
*)
(* it is always combined with a p_auth or authorized_age
   TODO : diff between p_auth and authorized_age *)
(* TODO why not conf.hide_names && not Util.is_public *)
(* bug: is_hide_names if true if person is Private but with age > private_year *)
let is_hide_names conf p =
  conf.Config.hide_private_names
  && not (conf.Config.wizard || conf.Config.friend)
  || Gwdb.get_access p = Def.Private

let is_restricted (conf : Config.config) base (ip : Gwdb.iper) =
  let fct p =
    (not (Gwdb.is_quest_string (Gwdb.get_surname p)))
    && (not (Gwdb.is_quest_string (Gwdb.get_first_name p)))
    && not (is_visible conf base p)
  in
  conf.Config.use_restrict && Gwdb.base_visible_get base fct ip

module NameVisibilityUtil : sig
  type name_visibility = HiddenName | RestrictedName | VisibleName

  val name_visibility_of_person :
    conf:Config.config ->
    base:Gwdb.base ->
    person:Gwdb.person ->
    name_visibility
end = struct
  type name_visibility = HiddenName | RestrictedName | VisibleName

  let is_hidden conf base person =
    is_hide_names conf person && not (is_visible conf base person)

  let name_visibility_of_person ~conf ~base ~person =
    if is_empty person then RestrictedName
    else if is_hidden conf base person then HiddenName
    else VisibleName
end

let is_hidden conf base person =
  NameVisibilityUtil.(
    name_visibility_of_person ~conf ~base ~person = HiddenName)

let has_restricted_name conf base person =
  NameVisibilityUtil.(
    name_visibility_of_person ~conf ~base ~person = RestrictedName)

let map_name_visibility ~on_hidden_name ~on_restricted_name ~on_visible_name
    ~conf ~base ~person =
  match NameVisibilityUtil.name_visibility_of_person ~conf ~base ~person with
  | NameVisibilityUtil.HiddenName -> on_hidden_name conf base person
  | RestrictedName -> on_restricted_name conf base person
  | VisibleName -> on_visible_name conf base person

let has_visible_name conf base person =
  map_name_visibility
    ~on_hidden_name:(fun _ _ _ -> false)
    ~on_restricted_name:(fun _ _ _ -> false)
    ~on_visible_name:(fun _ _ _ -> true)
    ~conf ~base ~person
