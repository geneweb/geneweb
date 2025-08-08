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

(** Calcul les droits de visualisation d'une personne en fonction de son age.
    Renvoie (dans l'ordre des tests) :
    - Vrai si : magicien ou ami ou la personne est public
    - Vrai si : la personne est en si_titre, si elle a au moins un titre et que
      public_if_title = yes dans le fichier gwf
    - Faux si : la personne n'est pas décédée et private_years > 0
    - Vrai si : la personne est plus agée (en fonction de la date de naissance
      ou de la date de baptème) que privates_years
    - Faux si : la personne est plus jeune (en fonction de la date de naissance
      ou de la date de baptème) que privates_years
    - Vrai si : la personne est décédée depuis plus de privates_years
    - Faux si : la personne est décédée depuis moins de privates_years
    - Vrai si : la personne a entre 80 et 120 ans et qu'elle n'est pas privée et
      public_if_no_date = yes
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
