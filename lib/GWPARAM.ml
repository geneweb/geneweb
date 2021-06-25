(** This module allows plugins to modify geneweb configuration.

    This approch is preffered if it does not come with a performance
    cost.
*)

let init = ref @@ fun () ->
  List.fold_right Filename.concat [ Gwlib.prefix ; "share" ] "geneweb"
  |> Secure.add_assets ;
  Secure.add_assets Filename.current_dir_name

let base_path = ref @@ fun pref bname ->
  List.fold_right Filename.concat (Secure.bd () :: pref) bname

let bpath = ref @@ fun bname -> Filename.concat (Secure.bd ()) bname

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
let p_auth = ref @@ fun conf base p ->
  conf.Config.wizard
  || conf.friend
  || Gwdb.get_access p = Public
  || (conf.public_if_titles
      && Gwdb.get_access p = IfTitles
      && Gwdb.nobtit base conf.allowed_titles conf.denied_titles p <> [])
  || begin
    let death = Gwdb.get_death p in
    if death = NotDead then conf.private_years < 1
    else
      let check_date d none = match d with
        | Some (Def.Dgreg (d, _)) ->
          let a  = Date.time_elapsed d conf.today in
          if a.Def.year > conf.Config.private_years then true
          else if a.year < conf.private_years then false
          else a.month > 0 || a.day > 0
        | _ -> none ()
      in
      check_date (Gwdb.get_birth p |> Adef.od_of_cdate) @@ fun () ->
      check_date (Gwdb.get_baptism p |> Adef.od_of_cdate) @@ fun () ->
      check_date (Gwdb.get_death p |> Date.date_of_death) @@ fun () ->
      (Gwdb.get_access p <> Def.Private && conf.public_if_no_date)
      || begin
        let families = Gwdb.get_family p in
        let len = Array.length families in
        let rec loop i =
          i < len
          && check_date
            (Array.get families i
             |> Gwdb.foi base
             |> Gwdb.get_marriage
             |> Adef.od_of_cdate)
            (fun () -> loop (i + 1))
        in
        loop 0
      end
  end
