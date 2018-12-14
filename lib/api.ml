#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

module Mapp = Api_app_piqi
module Mext_app = Api_app_piqi_ext

open Config
open Def
open Gwdb
open Util
open Api_def
open Api_util

(**/**) (* Services disponibles. *)


(* ******************************************************************** *)
(*  [Fonc] print_info_base : config -> base -> InfosBase                *)
(** [Description] : Retourne les informations d'une base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - string : Mets à jour les champs du type Infos_base.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_info_base conf base =
  let (sosa_p, sosa) =
     match Util.find_sosa_ref conf base with
     | Some p ->
         let fn = Name.lower (sou base (get_first_name p)) in
         let sn = Name.lower (sou base (get_surname p)) in
         let occ = Int32.of_int (get_occ p) in
         let sosa =
           Some M.Reference_person.({
             n = sn;
             p = fn;
             oc = occ;
           })
         in
         (Some p, sosa)
     | None -> (None, None)
  in
  let sosa_or_first_ind =
    match sosa_p with
    | Some p -> Int64.of_int (Adef.int_of_iper (get_key_index p))
    | None -> Int64.of_int 0
  in
  let last_modified_person =
    try
      let ic = Secure.open_in_bin (History.file_name conf) in
      let (_, pos, wiz) = (1, in_channel_length ic, "") in
      let vv = (ref (Bytes.create 0), ref 0) in
      let last_modified_person =
        let (line, _) = History.rev_input_line ic pos vv in
        match History.line_fields line with
        | Some (_, user, action, keyo) ->
          if wiz = "" || user = wiz then
            match keyo with
            | Some key ->
              (match action with
               | "mn" -> sosa_or_first_ind
               | _ ->
                 (match Gutil.person_ht_find_all base key with
                  | [ip] -> Int64.of_int (Adef.int_of_iper ip)
                  | _ -> sosa_or_first_ind))
            | None -> sosa_or_first_ind
          else sosa_or_first_ind
        | None -> sosa_or_first_ind
      in
      close_in ic;
      last_modified_person
    with Sys_error _ | _ -> sosa_or_first_ind
  in
  let last_modified_person =
    if Gwdb.nb_of_persons base = 0 then None
    else Some last_modified_person
  in
  let info_base =
    M.Infos_base.({
      nb_persons = Int64.of_int (Gwdb.nb_of_persons base);
      nb_families = Int64.of_int (Gwdb.nb_of_families base);
      sosa = sosa;
      last_modified_person = last_modified_person;
      real_nb_persons = Some (Int64.of_int (Util.real_nb_of_persons conf base));
    })
  in
  let data = Mext.gen_infos_base info_base in
  print_result conf data
;;


(* ******************************************************************** *)
(*  [Fonc] print_loop : config -> base -> Person                        *)
(** [Description] : Si il y a une boucle dans la base, retourne la
                    personne qui est son propre ancêtre, une personne
                    vide sinon.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - Person : La personne qui est son propre ancêtre.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_loop conf base =
  let (base_loop, pers) =
    (ref false, ref (poi base (Adef.iper_of_int (-1))))
  in
  (* On ne fait pas un Util.create_topological_sort conf base qui est certe *)
  (* plus rapide, mais qui dans de rare cas, n'est pas capable de remonter  *)
  (* la boucle (on ne check pas la base en entier). Avec cette méthode, on  *)
  (* n'a pas de ce problème.                                                *)
  let () =
    Consang.check_noloop base
      (function
        | OwnAncestor p -> base_loop := true; pers := p
        | _ -> () )
  in
  let p =
    if !base_loop then
      (* Comme il y a une boucle, Perso.get_single_sosa ne va pas marcher *)
      (* mais la fonction ne sera pas appelée dans pers_to_piqi_person.   *)
      pers_to_piqi_person conf base !pers !base_loop
        (Perso.get_single_sosa conf base) false
    else
      let ref_pers =
        M.Reference_person.({
          n = "";
          p = "";
          oc = Int32.of_int 0;
        })
      in
      empty_piqi_person conf ref_pers false
  in
  let data = data_person p in
  print_result conf data
;;


(* ******************************************************************** *)
(*  [Fonc] print_info_ind : config -> base -> unit                      *)
(** [Description] : Renvoie à partir d'une référence person (piqi) une
                    person (piqi).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_info_ind conf base =
  let ref_person = get_params conf Mext.parse_reference_person in
  let filters = get_filters conf in
  let base_loop = has_base_loop conf base in
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  let p =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
        let p = pget conf base ip in
        if apply_filters_p conf filters (Perso.get_single_sosa conf base) p then
          pers_to_piqi_person
            conf base p base_loop (Perso.get_single_sosa conf base) false
        else
          empty_piqi_person conf ref_person base_loop
    | None -> empty_piqi_person conf ref_person base_loop
  in
  let data = data_person p in
  print_result conf data
;;


(* ******************************************************************** *)
(*  [Fonc] print_list_ref_person : config -> base -> unit               *)
(** [Description] : Renvoie à partir d'une liste de référence person
                    (piqi) une liste de persons (piqi).
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_list_ref_person conf base =
  let list_ref_person = get_params conf Mext.parse_list_reference_persons in
  let filters = get_filters conf in
  let pl =
    List.map
      (fun ref_p ->
        let sn = ref_p.M.Reference_person.n in
        let fn = ref_p.M.Reference_person.p in
        let occ = ref_p.M.Reference_person.oc in
        match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
        | Some ip ->
            let p  = pget conf base ip in
            PFull p
        | None -> PLight ref_p )
      list_ref_person.M.List_reference_persons.list_ref_persons
  in
  let data =
    data_list_person_option conf base filters pl
  in
  print_result conf data
;;


(* ******************************************************************** *)
(*  [Fonc] print_ref_person_from_ip : config -> base -> unit            *)
(** [Description] : Renvoie à partir de l'ip une référence personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_ref_person_from_ip conf base =
  let id = get_params conf Mext.parse_index in
  let ip = Int32.to_int id.M.Index.index in
  let p = poi base (Adef.iper_of_int ip) in
  let fn = Name.lower (sou base (get_first_name p)) in
  let sn = Name.lower (sou base (get_surname p)) in
  let occ = Int32.of_int (get_occ p) in
  let ref_p =
    M.Reference_person.({
      n = sn;
      p = fn;
      oc = occ;
    })
  in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data
;;


(**/**) (* API_FIRST_AVAILABLE_PERSON *)

(* ************************************************************************ *)
(*  [Fonc] print_first_available_person : config -> base -> ReferencePerson *)
(** [Description] : Retourne la "première" personne accessible d'un arbre
                    et visible.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : ReferencePerson
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_first_available_person conf base =
  let empty_ref =
    M.Reference_person.({
      n = "";
      p = "";
      oc = Int32.of_int 0;
    })
  in
  let rec loop i nb_ind =
    if i = nb_ind - 1 then empty_ref
    else
      let p = poi base (Adef.iper_of_int i) in
      if is_hide_names conf p || is_empty_or_quest_name p ||
         not (authorized_age conf base p)
      then
        loop (i + 1) nb_ind
      else
        let fn = Name.lower (sou base (get_first_name p)) in
        let sn = Name.lower (sou base (get_surname p)) in
        let occ = Int32.of_int (get_occ p) in
        M.Reference_person.({
          n = sn;
          p = fn;
          oc = occ;
        })
  in
  let nb_ind = Gwdb.nb_of_persons base in
  let ref_p =
    if nb_ind = 0 then empty_ref
    else loop 0 nb_ind
  in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data
;;


(**/**) (* API_SOSA *)

(* ************************************************************************ *)
(*  [Fonc] print_find_sosa : config -> base -> ReferencePerson              *)
(** [Description] : Cette fonction est utilisée pour la première saisie.
       Elle prend une référence_person et si elle a des enfants, alors on
       renvoi le premier enfant, sinon on renvoi la même personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : ReferencePerson
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_find_sosa conf base =
  let ref_person = get_params conf Mext.parse_reference_person in
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  let ref_p =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
        let p = poi base ip in
        let rec loop faml =
          match faml with
          | [] ->
              (* On reconstruit la ref_person pour être sûr des accents. *)
              M.Reference_person.({
                n = Name.lower sn;
                p = Name.lower fn;
                oc = occ;
              })
          | ifam :: faml ->
              let fam = foi base ifam in
              match (Array.to_list (get_children fam)) with
              | [] -> loop faml
              | ip :: _ ->
                  let sosa = poi base ip in
                  let fn = Name.lower (sou base (get_first_name sosa)) in
                  let sn = Name.lower (sou base (get_surname sosa)) in
                  let occ = Int32.of_int (get_occ sosa) in
                  M.Reference_person.({
                    n = sn;
                    p = fn;
                    oc = occ;
                  })
        in
        loop (Array.to_list (get_family p))
    | None ->
        M.Reference_person.({
          n = "";
          p = "";
          oc = Int32.of_int 0;
        })
  in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data
;;


(**/**) (* API_LAST_MODIFIED_PERSONS *)

(* ******************************************************************** *)
(*  [Fonc] print_last_modified_persons : config -> base -> persons list *)
(** [Description] : Retourne la liste des dernières personnes modifiées
                    par le magicien. Si aucun magicien n'est donné, alors
                    c'est les dernières personnes.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - persons list : List des personnes modifiées.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_last_modified_persons conf base =
  let params = get_params conf Mext.parse_last_modifications in
  let filters = get_filters conf in
  let wiz =
    match params.M.Last_modifications.wizard with
    | Some wiz -> wiz
    | None -> ""
  in
  let max_res =
    match params.M.Last_modifications.max_res with
    | Some i -> Int32.to_int i
    | None -> 10
  in
  let range =
    match params.M.Last_modifications.range with
    | Some range ->
        let date_begin = range.M.Filter_date_range.date_begin in
        let dmy1 =
          { day = Int32.to_int date_begin.M.Filter_date.day;
            month = Int32.to_int date_begin.M.Filter_date.month;
            year = Int32.to_int date_begin.M.Filter_date.year;
            prec = Sure; delta = 0 }
        in
        let date_end = range.M.Filter_date_range.date_end in
        let dmy2 =
          { day = Int32.to_int date_end.M.Filter_date.day;
            month = Int32.to_int date_end.M.Filter_date.month;
            year = Int32.to_int date_end.M.Filter_date.year;
            prec = Sure; delta = 0 }
        in
        let prec = range.M.Filter_date_range.only_exact in
        Some (dmy1, dmy2, prec)
    | None -> None
  in
  let is_time_included time =
    match range with
    | Some (date_begin, date_end, prec) ->
        (* time : 0000-00-00 00:00:00 *)
        let date =
          let y = int_of_string (String.sub time 0 4) in
          let m = int_of_string (String.sub time 5 2) in
          let d = int_of_string (String.sub time 8 2) in
          let dmy =
            { day = d; month = m; year = y; prec = Sure; delta = 0; }
          in
          Some (Dgreg (dmy, Dgregorian))
        in
        is_date_included prec date date_begin date_end
    | None -> true
  in
  let p_mem ip list =
    let rec loop list =
      match list with
      | [] -> false
      | p :: list ->
          if ip = get_key_index p then true
          else loop list
    in
    loop list
  in
  let compute_sosa = Perso.get_single_sosa conf base in
  let list =
    match
      try Some (Secure.open_in_bin (History.file_name conf))
      with Sys_error _ -> None
    with
    | Some ic ->
        let pos = in_channel_length ic in
        let vv = (ref (Bytes.create 0), ref 0) in
        let rec loop list res pos =
          if res = 0 then list
          else
            match
              try Some (History.rev_input_line ic pos vv)
              with History.Begin_of_file -> None
            with
            | Some (line, pos) ->
                (match History.line_fields line with
                | Some (time, user, action, keyo) ->
                    if (wiz = "" || user = wiz) && is_time_included time then
                      match keyo with
                      | Some key ->
                          (match action with
                          | "mn" | "dp" | "cp" | "cs" | "co" ->
                              loop list res pos
                          | _ ->
                              (match Gutil.person_ht_find_all base key with
                              | [ip] ->
                                  let p = poi base ip in
                                  if not (is_empty_or_quest_name p) &&
                                     apply_filters_p
                                       conf filters compute_sosa p &&
                                     not (p_mem ip list)
                                  then loop (p :: list) (res - 1) pos
                                  else loop list res pos
                              | _ -> loop list res pos))
                      | None -> loop list res pos
                    else loop list res pos
                | None -> loop list res pos)
            | None -> list
        in
        let list = loop [] max_res pos in
        close_in ic;
        List.rev list
    | None -> []
  in
  let data = conv_data_list_person conf base filters list in
  print_result conf data
;;


(**/**) (* API_LAST_VISITED_PERSONS *)

(* ************************************************************************ *)
(*  [Fonc] print_last_visited_persons : config -> base -> persons list      *)
(** [Description] : Retourne la liste des dernières personnes visités
                    par le user donné en paramètre.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - persons list : Liste des personnes modifiées.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_last_visited_persons conf base =
  let last_visits = get_params conf Mext.parse_last_visits in
  let user = last_visits.M.Last_visits.user in
  let filters = get_filters conf in
  let list =
    if user = "" then []
    else
      let ht = Util.read_visited conf in
      try Hashtbl.find ht user with
      Not_found -> []
  in
  (* On ne supprime pas le fichier de cache, même après un envoi Gendcom, *)
  (* donc on vérifie que les personnes existent toujours dans la base.    *)
  let list =
    List.fold_right
      (fun (ip, _) accu -> try poi base ip :: accu with _ -> accu)
      list []
  in
  let data = data_list_person conf base filters list in
  print_result conf data
;;


(**/**) (* API_MAX_ANCESTORS *)

(* ************************************************************************ *)
(*  [Fonc] print_max_ancestors : config -> base -> ReferencePerson          *)
(** [Description] : Recherche la personne qui a le plus d'ancêtres.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : ReferencePerson
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_max_ancestors conf base =
  let module IperSet =
        Set.Make
          (struct
            type t = iper;;
            let compare i1 i2 =
              Pervasives.compare (Adef.int_of_iper i1) (Adef.int_of_iper i2);;
           end)
  in

  let nb_ind = nb_of_persons base in
  let ancestors = Array.make nb_ind IperSet.empty in
  let mark = Array.make nb_ind false in

  let has_children ip =
    let p = poi base ip in
    let fam = get_family p in
    let rec loop fam =
      match fam with
      | [] -> false
      | ifam :: l ->
          let des = foi base ifam in
          if Array.length (get_children des) > 0 then true
          else loop l
    in
    loop (Array.to_list fam)
  in

  let rec nb_ancestors ip =
    if mark.(Adef.int_of_iper ip) then ancestors.(Adef.int_of_iper ip)
    else
      begin
        let anc =
          match get_parents (poi base ip) with
          | Some ifam ->
              let cpl = foi base ifam in
              let anc =
                IperSet.add (get_father cpl) ancestors.(Adef.int_of_iper ip)
              in
              let anc =
                IperSet.add (get_mother cpl) anc
              in
              let anc2 =
                IperSet.union
                  (nb_ancestors (get_father cpl))
                  (nb_ancestors (get_mother cpl))
              in
              IperSet.union anc anc2
          | None -> IperSet.empty
        in
        ancestors.(Adef.int_of_iper ip) <- anc;
        mark.(Adef.int_of_iper ip) <- true;
        anc
      end
  in

  (*
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let () = load_descends_array base in
  let () = load_unions_array base in
  *)
  let rec loop i =
    if i = nb_ind then ()
    else if has_children (Adef.iper_of_int i) then loop (succ i)
    else if mark.(i) then loop (succ i)
    else
      begin
        let anc = nb_ancestors (Adef.iper_of_int i) in
        ancestors.(i) <- anc;
        mark.(i) <- true;
        loop (succ i)
      end
  in
  let () = loop 0 in
  (* ip, nb_anc *)
  let res = ref (0, 0) in
  for i = 0 to nb_ind - 1 do
    let nb = IperSet.cardinal ancestors.(i) in
    if nb > snd !res then res := (i, nb)
    else ()
  done;
  let p = poi base (Adef.iper_of_int (fst !res)) in
  let fn = Name.lower (sou base (get_first_name p)) in
  let sn = Name.lower (sou base (get_surname p)) in
  let occ = Int32.of_int (get_occ p) in
  let ref_p =
    M.Reference_person.({
      n = sn;
      p = fn;
      oc = occ;
    })
  in
  let data = Mext.gen_reference_person ref_p in
  print_result conf data
;;


(**/**) (* API_IMAGE *)

let print_img conf base =
  let filters = get_filters conf in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    begin
      let l = ref [] in
      let base_loop = has_base_loop conf base in
      let () = Perso.build_sosa_ht conf base in
      let () = load_image_ht conf in
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        match Api_util.find_image_file conf base p with
        | Some f ->
            let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true in
            l := M.Full_image.({person = p; img = f;}) :: !l
        | None -> ()
      done;
      if filters.nb_results then
        let len = M.Internal_int32.({value = Int32.of_int (List.length !l)}) in
        let data = Mext.gen_internal_int32 len in
        print_result conf data
      else
        let list = M.List_full_images.({images = !l}) in
        let data = Mext.gen_list_full_images list in
        print_result conf data
    end
  else
    begin
      let l = ref [] in
      let base_loop = has_base_loop conf base in
      let () = Perso.build_sosa_ht conf base in
      let () = load_image_ht conf in
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        match Api_util.find_image_file conf base p with
        | Some f ->
            let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person true in
            l := M.Image.({person = p; img = f;}) :: !l
        | None -> ()
      done;
      if filters.nb_results then
        let len = M.Internal_int32.({value = Int32.of_int (List.length !l)}) in
        let data = Mext.gen_internal_int32 len in
        print_result conf data
      else
        let list = M.List_images.({list_images = !l}) in
        let data = Mext.gen_list_images list in
        print_result conf data
    end
;;

(**/**) (* API_IMAGE_EXT *)

let print_img_ext conf base =
  let filters = get_filters conf in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    begin
      let l = ref [] in
      let base_loop = has_base_loop conf base in
      let () = Perso.build_sosa_ht conf base in
      let () = load_image_ht conf in
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        let http = "http://" in
        let img = sou base (get_image p) in
        if not (is_empty_string (get_image p)) &&
             String.length img > String.length http &&
               String.sub img 0 (String.length http) = http
        then
          let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true in
          l := M.Full_image.({person = p; img = img;}) :: !l
      done;
      if filters.nb_results then
        let len = M.Internal_int32.({value = Int32.of_int (List.length !l)}) in
        let data = Mext.gen_internal_int32 len in
        print_result conf data
      else
        let list = M.List_full_images.({images = !l}) in
        let data = Mext.gen_list_full_images list in
        print_result conf data
    end
  else
    begin
      let l = ref [] in
      let base_loop = has_base_loop conf base in
      let () = Perso.build_sosa_ht conf base in
      let () = load_image_ht conf in
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        let http = "http://" in
        let img = sou base (get_image p) in
        if not (is_empty_string (get_image p)) &&
             String.length img > String.length http &&
               String.sub img 0 (String.length http) = http
        then
          let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person true in
          l := M.Image.({person = p; img = img;}) :: !l
      done;
      if filters.nb_results then
        let len = M.Internal_int32.({value = Int32.of_int (List.length !l)}) in
        let data = Mext.gen_internal_int32 len in
        print_result conf data
      else
        let list = M.List_images.({list_images = !l}) in
        let data = Mext.gen_list_images list in
        print_result conf data
    end
;;


(**/**) (* API_IMAGE_ALL *)

let print_img_all conf base =
  let filters = get_filters conf in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    begin
      let l = ref [] in
      let base_loop = has_base_loop conf base in
      let () = Perso.build_sosa_ht conf base in
      (* On commente pour la migration des portraits *)
      (*let () = load_image_ht conf in*)
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        if not (is_empty_string (get_image p)) then
          let img = sou base (get_image p) in
          (* On commente pour la migration des portraits true => false *)
          let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person false in
          l := M.Full_image.({person = p; img = img;}) :: !l
        else
          match Api_util.find_image_file conf base p with
          | Some f ->
              let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person false in
              l := M.Full_image.({person = p; img = f;}) :: !l
          | None -> ()
      done;
      if filters.nb_results then
        let len = M.Internal_int32.({value = Int32.of_int (List.length !l)}) in
        let data = Mext.gen_internal_int32 len in
        print_result conf data
      else
        let list = M.List_full_images.({images = !l}) in
        let data = Mext.gen_list_full_images list in
        print_result conf data
    end
  else
    begin
      let l = ref [] in
      let base_loop = has_base_loop conf base in
      let () = Perso.build_sosa_ht conf base in
      (* On commente pour la migration des portraits *)
      (*let () = load_image_ht conf in*)
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        if not (is_empty_string (get_image p)) then
          let img = sou base (get_image p) in
          let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person false in
          l := M.Image.({person = p; img = img;}) :: !l
        else
          match Api_util.find_image_file conf base p with
          | Some f ->
              let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person false in
              l := M.Image.({person = p; img = f;}) :: !l
          | None -> ()
      done;
      if filters.nb_results then
        let len = M.Internal_int32.({value = Int32.of_int (List.length !l)}) in
        let data = Mext.gen_internal_int32 len in
        print_result conf data
      else
        let list = M.List_images.({list_images = !l}) in
        let data = Mext.gen_list_images list in
        print_result conf data
    end
;;


(**/**) (* API_IMAGE_APP *)

let print_img_person conf base =
  let id = get_params conf Mext.parse_index in
  let ip = Int32.to_int id.M.Index.index in
  let p = poi base (Adef.iper_of_int ip) in
  let img_addr =
    match sou base (get_image p) with
    | "" ->
        (match Util.auto_image_file conf base p with
        | Some file -> file
        | None -> "")
    | s -> s
  in
  let img_from_ip = M.Image_address.({img = img_addr}) in
  let data = Mext.gen_image_address img_from_ip in
  print_result conf data
;;



(**/**) (* API_UPDT_IMAGE *)

let print_updt_image conf base =
  let pers_img_l = get_params conf Mext.parse_list_pers_img in
  let pers_img_l = pers_img_l.M.List_pers_img.list_pers_img in
  List.iter
    (fun pers_img ->
      let pers = pers_img.M.Pers_img.person in
      let sn = pers.M.Reference_person.n in
      let fn = pers.M.Reference_person.p in
      let occ = pers.M.Reference_person.oc in
      let img = pers_img.M.Pers_img.img in
      match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
      | Some ip ->
          let p = poi base ip in
          let p =
            {(gen_person_of_person p) with image = Gwdb.insert_string base img}
          in
          patch_person base p.key_index p
      | None -> () )
    pers_img_l;
  Gwdb.commit_patches base;
;;


(**/**) (* API_REMOVE_IMAGE_EXT *)

let print_remove_image_ext base =
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let http = "http://" in
    let img = sou base (get_image p) in
    let is_ext =
      String.length img > String.length http &&
        String.sub img 0 (String.length http) = http
    in
    if img <> "" && is_ext then
      let p =
        {(gen_person_of_person p) with image = Gwdb.insert_string base ""}
      in
      patch_person base p.key_index p
  done;
  Gwdb.commit_patches base
;;


(**/**) (* API_REMOVE_IMAGE_EXT_ALL *)

let print_remove_image_ext_all base =
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if not (is_empty_string (get_image p)) then
      let p =
        {(gen_person_of_person p) with image = Gwdb.insert_string base ""}
      in
      patch_person base p.key_index p
  done;
  Gwdb.commit_patches base
;;



(**/**) (* API_ANNIVERSARY => DEPRECATED *)

(* ************************************************************************* *)
(*  [Fonc] f_scan : config -> base -> unit -> person                         *)
(** [Description] : Renvoie la personne suivante lors du parcours de la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : person
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let f_scan conf base =
  let i = ref (-1) in
  fun () ->
    begin
      incr i;
      if !i < nb_of_persons base then
        pget conf base (Adef.iper_of_int !i)
      else raise Not_found
    end
;;


(* ********************************************************************* *)
(*  [Fonc] build_anniversary_pers_list :
      config -> base -> int -> (unit -> person) -> person list           *)
(** [Description] : Construit la liste des personnes dont c'est
                    l'anniversaire pour le mois donné. Renvoie la liste
                    des personnes triée par âge.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : person list
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let build_anniversary_pers_list conf base month f_scan =
  let tab = Array.make 31 [] in
  (try
    while true do
      let p = f_scan () in
      match (Adef.od_of_cdate (get_birth p), get_death p) with
      | (Some (Dgreg (d, _)), (NotDead | DontKnowIfDead)) ->
          if d.prec = Sure && d.day <> 0 && d.month <> 0 &&
             d.month = month && d.delta = 0
          then
            if authorized_age conf base p then
              let j = d.day in
              tab.(pred j) <- ((p, d.year) :: tab.(pred j))
      | _ -> ()
    done
  with Not_found -> ());
  let tab =
    Array.map
      (fun l -> List.sort (fun (_, a1) (_, a2) -> compare a1 a2) l )
      tab
  in
  let list = Array.to_list tab in
  let list = List.map (fun (p, _) -> p) (List.flatten list) in
  reduce_to_recent conf list
;;


(* ********************************************************************* *)
(*  [Fonc] print_birthday : config -> base -> ListPersons                *)
(** [Description] : Récupère en paramètre de la requête le mois, et
      renvoie la liste des personnes dont c'est l'anniversaire ce mois ci.
      Si aucun mois n'est donnée, on prend le mois courrant.
      DEPRECATED : use all_person + filter
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - person list : Retourne la liste des personnes dont c'est
                      l'anniversaire.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_birthday conf base =
  let an = get_params conf Mext.parse_anniversary_params in
  let filters = get_filters conf in
  let month =
    match an.M.Anniversary_params.month with
    | Some n ->
        let n = Int32.to_int n in
        if n < 1 then 1
        else if n > 12 then 12
        else n
    | None -> conf.today.month
  in
  let list = build_anniversary_pers_list conf base month (f_scan conf base) in
  let data = data_list_person conf base filters list in
  print_result conf data
;;


(**/**) (* API_CHECK_BASE *)


(* ********************************************************************* *)
(*  [Fonc] print_base_warnings : config -> base -> unit                  *)
(** [Description] : Renvoie les listes des erreurs et warnings d'une base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - base_warning : Les listes de tous les warnings de la base.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_base_warnings conf base =
  let filters = get_filters conf in
  let errors = ref [] in
  let warnings = ref [] in
  Check.check_base base
    (Api_warnings.set_list errors) (Api_warnings.set_list warnings)
    (fun _ -> true) (fun _ -> ()) false;
  (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
  (* levé par plusieurs fonctions différents selon le context.       *)
  let warnings =
    let ht = Hashtbl.create 1 in
    let rec loop wl accu =
      match wl with
      | [] -> accu
      | x :: wl ->
          if Hashtbl.mem ht (Hashtbl.hash x) then loop wl accu
          else begin
            Hashtbl.add ht (Hashtbl.hash x) true;
            loop wl (x :: accu)
          end
    in
    loop !warnings []
  in
  let base_loop = has_base_loop conf base in
  let () = Perso.build_sosa_ht conf base in
  let () = load_image_ht conf in
  List.iter
    (Api_warnings.add_error_to_piqi_warning_list
       conf base base_loop Perso.get_sosa_person true)
    !errors;
  List.iter
    (Api_warnings.add_warning_to_piqi_warning_list
       conf base base_loop Perso.get_sosa_person true)
    warnings;
  (* On propage les modifications pour les warnings ChangedOrderOf... *)
  List.iter
    (fun warn ->
      (match warn with
      | ChangedOrderOfChildren (ifam, _, _, after) ->
          patch_descend base ifam {children = after}
      | ChangedOrderOfMarriages (p, _, after) ->
          patch_union base (get_key_index p) {family = after}
      | _ -> ()))
    warnings;
  (* Attention, les FLEX peuvent aussi faire un calcul de warning, *)
  (* mais on n'applique pas la modification de la base.            *)
  if conf.wizard then Util.commit_patches conf base
  else ();
  let data =
    if filters.nb_results then
      let len = List.length !errors + List.length warnings in
      let len = M.Internal_int32.({value = Int32.of_int len}) in
      Mext.gen_internal_int32 len
    else
      let base_warnings = Api_warnings.create_piqi_warnings () in
      Mext.gen_base_warnings base_warnings
  in
  print_result conf data
;;


(**/**) (* Récupération de toute une base. *)

let print_all_persons conf base =
  let params = get_params conf Mext.parse_all_persons_params in
  let filters = get_filters conf in
  let from = params.M.All_persons_params.from in
  let limit = params.M.All_persons_params.limit in
  let (from, limit) =
    match (from, limit) with
    | (Some f, Some l) -> (Int32.to_int f, Int32.to_int f + Int32.to_int l)
    | (Some f, None) -> (Int32.to_int f, nb_of_persons base)
    | (None, Some l) -> (0, Int32.to_int l)
    | (None, None) -> (0, nb_of_persons base)
  in
  let () = Perso.build_sosa_ht conf base in
  let list = ref [] in
  for i = from to limit - 1 do
    if i < nb_of_persons base then
      let p = poi base (Adef.iper_of_int i) in
      list := p :: !list
    else ()
  done;
  let data = data_list_person conf base filters (List.rev !list) in
  print_result conf data
;;


let print_all_families conf base =
  let params = get_params conf Mext.parse_all_families_params in
  let filters = get_filters conf in
  let from = params.M.All_families_params.from in
  let limit = params.M.All_families_params.limit in
  let nb_families = nb_of_families base in
  let (from, limit) =
    match (from, limit) with
    | (Some f, Some l) -> (Int32.to_int f, Int32.to_int f + Int32.to_int l)
    | (Some f, None) -> (Int32.to_int f, nb_families)
    | (None, Some l) -> (0, Int32.to_int l)
    | (None, None) -> (0, nb_families)
  in
  let list = ref [] in
  for i = limit - 1 downto from do
    if i < nb_families then
      let ifam = Adef.ifam_of_int i in
      list := fam_to_piqi_family conf base ifam :: !list
    else ()
  done;
  let data =
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length !list)}) in
      Mext.gen_internal_int32 len
    else
      let list = M.List_full_families.({families = !list}) in
      Mext.gen_list_full_families list
  in
  print_result conf data
;;


module StringMap =
  Map.Make
    (struct
      type t = string;;
      let compare = Gutil.alphabetic_order ;;
     end)
;;

module IperSort =
  Set.Make
    (struct
      type t = string * string ;;
      let compare (sn1, fn1) (sn2, fn2) =
        let cmp = compare sn1 sn2 in
        if cmp = 0 then compare fn1 fn2
        else cmp;;
(*
        let cmp = Gutil.alphabetic_order sn1 sn2 in
        if cmp = 0 then Gutil.alphabetic_order fn1 fn2
        else cmp;;
*)
     end)
;;

let print_all_full_person conf base =
  let base_loop = has_base_loop conf base in
  let () = Perso.build_sosa_ht conf base in
  let fname = "/tmp/pb_base_data.log" in
  let fname2 = "/tmp/pb_base_index.log" in
  let fname3 = "/tmp/pb_base_index_surname.log" in
  let () = load_ascends_array base in
  let () = load_strings_array base in
  let () = load_couples_array base in
  let () = load_unions_array base in
  let () = load_descends_array base in
  let () = load_image_ht conf in
(*  let l = ref IperSort.empty in*)
  let index_map = ref StringMap.empty in
  let add_to_map k v =
    try
      let l = StringMap.find k !index_map in
      index_map := StringMap.add k (v :: l) !index_map
    with Not_found -> index_map := StringMap.add k [v] !index_map
  in

  (try
    let oc = open_out_bin fname in
    let oc_index = open_out_bin fname2 in
    let oc_index_surname = open_out_bin fname3 in
    output_binary_int oc (nb_of_persons base);
    output_binary_int oc (nb_of_families base);
    output_binary_int oc 16;
    output_binary_int oc 0;
    let curr = ref 0 in
    for i = 0 to nb_of_persons base - 1 do
      let p = poi base (Adef.iper_of_int i) in
      let sn = sou base (get_surname p) in
      add_to_map sn i;
      (*      let fn = sou base (get_first_name p) in*)
      (*      l := IperSort.add (sn, fn) !l;*)
      let p =
        pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true
      in
      let data = Mext.gen_full_person p in
      let data = data `pb in
      Printf.fprintf oc "%s" data;
      output_binary_int oc_index !curr;
      curr := !curr + String.length data;
    done;
    flush oc;
    seek_out oc 12;
    output_binary_int oc (!curr + 16);
    seek_out oc (!curr + 16);
    curr := 0;
    for i = 0 to nb_of_families base - 1 do
      let fam = fam_to_piqi_family conf base (Adef.ifam_of_int i) in
      let data = Mext.gen_full_family fam in
      let data = data `pb in
      Printf.fprintf oc "%s" data;
      output_binary_int oc_index !curr;
      curr := !curr + String.length data;
    done;
    flush oc;
    (*
      let list = IperSort.elements !l in
      List.iter
      (fun (sn, fn) -> Printf.fprintf oc "%s %s\n" sn fn)
      list;
      flush oc;
    *)

    StringMap.iter
      (fun _ v -> List.iter (output_binary_int oc_index_surname) v)
      !index_map;

(*
    StringMap.iter
      (fun k v -> List.iter (fun i -> print_endline (string_of_int i)) v)
      !index_map;
    let _ = print_endline "\n\n" in
*)

    close_out oc;
    close_out oc_index;
    close_out oc_index_surname;
  with Sys_error _ -> print_endline "error !!!");

(*
  (* On test ce qu'on a écrit... *)
  (* lecture des noms par ordre alphabetique qui pointent vers des personnes *)
  (try
    let ic = open_in_bin fname in
    let ic_index = open_in_bin fname2 in
    let ic_index_surname = open_in_bin fname3 in

    let surnamel = ref [] in

    (try
      begin
        while true do
          surnamel := input_binary_int ic_index_surname :: !surnamel
        done;
        close_in ic_index_surname
      end
    with End_of_file -> ());

    surnamel := List.rev !surnamel;

    seek_in ic 0;
    let nb_person = input_binary_int ic in
    let size = 4 * nb_person in


    List.iter
      (fun i_sn ->
(*        let _ = print_endline ("i_sn = " ^ string_of_int i_sn) in*)
        seek_in ic_index (i_sn * 4);
        let i_p = input_binary_int ic_index in
        let i_p2 =
          if ((i_sn + 1) * 4) < size then
            input_binary_int ic_index
          else
            begin
              seek_in ic 12;
              input_binary_int ic - 16
            end
        in
(*        let _ = print_endline ("i_p = " ^ string_of_int i_p) in*)
(*        let _ = print_endline ("i_p2 = " ^ string_of_int i_p2) in*)
        let buff = String.create (i_p2 - i_p) in
(*        let _ = print_endline ("buff = " ^ string_of_int (String.length buff)) in*)
        seek_in ic (i_p + 16);
        (* en fait c'est mieux really_input ... mais pourquoi ... *)
        let _ = really_input ic buff 0 (String.length buff) in
(*        let _ = print_endline buff in*)
        let ppp = Mext.parse_full_person buff `pb in
        print_endline (ppp.M.Full_person.lastname))
      !surnamel;

    close_in ic_index;
    close_in ic;

  with Sys_error _ -> print_endline "error 2 !!!");
*)

  Util.html conf ;
  Wserver.printf "Gagné !!!";
;;


(**/**) (* Version app *)

module NameSort =
  Set.Make
    (struct
      type t = int * string * string * string * Def.date option
      let compare (i1, sn1, fn1, _, d1) (i2, sn2, fn2, _, d2) =
        if sn1 = sn2 then
          if fn1 = fn2 then
            match (d1, d2) with
            | (Some d1, Some d2) ->
                if CheckItem.strictly_after d1 d2 then 1
                else
                  if CheckItem.strictly_after d2 d1 then -1
                  else compare i1 i2
            | (Some _, None) -> -1
            | (None, Some _) -> 1
            | _ -> compare i1 i2
          else compare fn1 fn2
        else compare sn1 sn2;;
     end)
;;

module NameSortMap =
  Map.Make
    (struct
      type t = string ;;
      let compare = compare ;;
     end)
;;

let intSetTab = ref (Array.make 1 0);;

module IntSet =
  Set.Make
    (struct
      type t = int;;
      let compare x y = compare !intSetTab.(x) !intSetTab.(y);;
     end)
;;


(*
   Fichier base_info :
     - nombre de personnes
     - nombre de familles
     - sosa de référence (1-num s'il existe, 0-0 sinon)
     - timestamp de la création de la base
*)
let print_export_info conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname = Filename.concat export_directory "pb_base_info.dat" in
  match try Some (open_out_bin fname) with Sys_error _ -> None with
  | Some oc ->
      output_binary_int oc (Util.real_nb_of_persons conf base);
      output_binary_int oc (nb_of_families base);
      let sosa_ref =
        match Util.find_sosa_ref conf base with
        | Some p ->
            (output_char oc '\001'; Adef.int_of_iper (get_key_index p))
        | None -> (output_char oc '\000'; 0)
      in
      output_binary_int oc sosa_ref;
      let timestamp = string_of_float (Unix.time ()) in
      let timestamp = String.sub timestamp 0 (String.index timestamp '.') in
      output_binary_int oc (String.length timestamp);
      output_string oc timestamp;
      (* Utilisation de Extlib pour le binaire. *)
      (* let timestamp = Int32.of_float (Unix.time ()) in *)
      (* IO.write_i32 oc timestamp; *)
      close_out oc;
  | None -> ()
;;


(*
   Fichier person index :
     - l'adresse dans le fichier data de cette personne
   Fichier person data :
     - offset delete : à la création, c'est la fin du fichier
     - liste des taille Person, Person (proto app)
*)
let print_export_person conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname_inx = Filename.concat export_directory "pb_base_person.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_person.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
      let curr = ref 0 in
      (* offset delete *)
      output_binary_int oc_dat 0;
      for i = 0 to nb_of_persons base - 1 do
        let ip = Adef.iper_of_int i in
        let p = poi base ip in
        let pers_app = pers_to_piqi_app_person conf base p in
        let data = Mext_app.gen_person pers_app in
        let data = data `pb in
        (* Longueur de la personne puis données de la personne *)
        output_binary_int oc_dat (String.length data);
        output_string oc_dat data;
        (* Adresse de la personne *)
        output_binary_int oc_inx !curr;
        (* Attention a ne pas oublier offset delete => +4 *)
        curr := !curr + 4 + String.length data;
      done;
      (* mise à jour de offset delete maintenant qu'on a fini *)
      seek_out oc_dat 0;
      output_binary_int oc_dat !curr;
      close_out oc_dat;
      close_out oc_inx;
  | _ -> ()
;;


(*
   Fichier family index :
    - l'adresse dans le fichier data de cette famille
   Fichier family data :
     - offset delete : à la création, c'est la fin du fichier
     - liste des taille Family, Family (proto app)
*)
let print_export_family conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname_inx = Filename.concat export_directory "pb_base_family.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_family.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
      let curr = ref 0 in
      (* offset delete *)
      output_binary_int oc_dat 0;
      for i = 0 to nb_of_families base - 1 do
        let ifam = Adef.ifam_of_int i in
        let fam_app = fam_to_piqi_app_family base ifam in
        let data = Mext_app.gen_family fam_app in
        let data = data `pb in
        (* Longueur de la famille puis données de la famille *)
        output_binary_int oc_dat (String.length data);
        output_string oc_dat data;
        (* Adresse de la famille *)
        output_binary_int oc_inx !curr;
        (* Attention a ne pas oublier offset delete => +4 *)
        curr := !curr + 4 + String.length data;
      done;
      (* mise à jour de offset delete maintenant qu'on a fini *)
      seek_out oc_dat 0;
      output_binary_int oc_dat !curr;
      close_out oc_dat;
      close_out oc_inx;
  | _ -> ()
;;


(*
   Fichier person_note index :
    - l'adresse dans le fichier data de cette personne
   Fichier person_note data :
     - note vide : elle se trouve en début de fichier
     - liste des notes individuelles
*)
let print_person_note conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname_inx = Filename.concat export_directory "pb_base_person_note.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_person_note.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
      (* Attention a ne pas oublier la note vide => +4 *)
      let curr = ref 4 in
      (* note vide *)
      output_binary_int oc_dat 0;
      for i = 0 to nb_of_persons base - 1 do
        let ip = Adef.iper_of_int i in
        let p = poi base ip in
        let data = sou base (get_notes p) in
        if data = "" then
          (* On pointe vers la note vide. *)
          output_binary_int oc_inx 0
        else
          begin
            (* Adresse de la personne *)
            output_binary_int oc_inx !curr;
            output_binary_int oc_dat (String.length data);
            output_string oc_dat data;
            curr := !curr + 4 + String.length data;
          end;
      done;
      close_out oc_dat;
      close_out oc_inx;
  | _ -> ()
;;


(*
   Fichier family_note index :
    - l'adresse dans le fichier data de cette famille
   Fichier family_note data :
     - note vide : elle se trouve en début de fichier
     - liste des notes familiales
*)
let print_family_note conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname_inx = Filename.concat export_directory "pb_base_family_note.inx" in
  let fname_dat = Filename.concat export_directory "pb_base_family_note.dat" in
  match
    try (Some (open_out_bin fname_inx), Some (open_out_bin fname_dat))
    with Sys_error _ -> (None, None)
  with
  | (Some oc_inx, Some oc_dat) ->
      (* Attention a ne pas oublier la note vide => +4 *)
      let curr = ref 4 in
      (* note vide *)
      output_binary_int oc_dat 0;
      for i = 0 to nb_of_families base - 1 do
        let ifam = Adef.ifam_of_int i in
        let fam = foi base ifam in
        let data = sou base (get_comment fam) in
        if data = "" then
          (* On pointe vers la note vide. *)
          output_binary_int oc_inx 0
        else
          begin
            (* Adresse de la personne *)
            output_binary_int oc_inx !curr;
            output_binary_int oc_dat (String.length data);
            output_string oc_dat data;
            curr := !curr + 4 + String.length data;
          end;
      done;
      close_out oc_dat;
      close_out oc_inx;
  | _ -> ()
;;


let export_img conf base =
  let prog = "convert" in
  (* Attention, en version finale ne pas garder le / 2 *)
  for i = 0 to (nb_of_persons base - 1) / 2 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let img = Util.auto_image_file conf base p in
    if not (is_empty_string (get_image p)) || img <> None then
      match img with
      | Some file ->
          let file = "/home/geneweb/bases/" ^ file in
          let dest = "/tmp/pb_base/img/" ^ string_of_int (i mod 10) ^ "/" in
          Mutil.mkdir_p dest;
          let pid =
            Unix.create_process
              prog
              [| prog;
                 "-resize"; "90x90"; file; dest ^ "/" ^ string_of_int i ^ ".jpg" |]
              Unix.stdin Unix.stdout Unix.stderr
          in
          ignore (Unix.waitpid [] pid)
      | None -> ()
  done
;;


(*
   Fichier name.inx :
    -
   Fichier name.wi :
    -
   Fichier name.w :
    -
   Fichier name.i :
    -
*)
let build_relative_name base p =
  let add_from_list accu list =
    List.fold_left
      (fun accu istr ->
        if is_empty_string istr then accu
        else Name.lower (sou base istr) :: accu)
      accu list
  in
  (* Nom de jeune fille *)
  (* Plus tard, en v2
  let list =
    if get_sex p = Female then
      List.fold_left
        (fun accu ifam ->
          let fam = foi base ifam in
          let isp = Gutil.spouse (get_key_index p) fam in
          let sp = poi base isp in
          let sn = Name.lower (sou base (get_surname sp)) in
          if sn = "" then accu else sn :: accu)
        [] (Array.to_list (get_family p))
    else []
  in
  *)
  let list = [] in
  let list =
    let pn = Name.lower (sou base (get_public_name p)) in
    if pn = "" then list else pn :: list
  in
  let list = add_from_list list (get_aliases p) in
  let list = add_from_list list (get_qualifiers p) in
  let list = add_from_list list (get_first_names_aliases p) in
  let list = add_from_list list (get_surnames_aliases p) in
  List.rev list
;;



let print_index_search conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let list_inx = ref NameSort.empty in
  let list_map = ref NameSortMap.empty in

  (* avec Hashtbl *)
  (*
  let ht = Hashtbl.create 5003 in

  let add_to_map k v =
    Hashtbl.add ht k v
  in
  *)

  (* avec Set *)
  let add_to_map k v =
    try
      let l = NameSortMap.find k !list_map in
      list_map := NameSortMap.add k (v :: l) !list_map
    with Not_found -> list_map := NameSortMap.add k [v] !list_map
  in

  let fname_inx = Filename.concat export_directory "pb_base_name.inx" in
  let fname_wi = Filename.concat export_directory "pb_base_name.wi" in
  let fname_w = Filename.concat export_directory "pb_base_name.w" in
  let fname_i = Filename.concat export_directory "pb_base_name.i" in

  begin
    try
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        if sn = "?" && fn = "?" then ()
        else
          begin
            let fn = Name.lower fn in
            let sn = Name.lower sn in
            let r = String.concat " " (build_relative_name base p) in
            let date =
              match (Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p)) with
              | (Some d1, _) -> Some d1
              | (_, Some d1) -> Some d1
              | _ -> None
            in
            list_inx := NameSort.add (i, sn, fn, r, date) !list_inx;
            (* Faut il faire si y'a plusieurs espaces ? *)
            (* FIXME: Does order matter or not? If not, use List.iter instead *)
            Util.rev_iter (fun sn -> add_to_map sn i) (String.split_on_char ' ' sn);
            Util.rev_iter (fun fn -> add_to_map fn i) (String.split_on_char ' ' fn);
            Util.rev_iter (fun n -> add_to_map n i) (String.split_on_char ' ' r);
          end
      done;

      intSetTab := Array.make (nb_of_persons base) 0;
      let nb_tab = ref 0 in

      let oc_name_inx = open_out_bin fname_inx in
      List.iter
        (fun (i, _, _, _, _) ->
          output_binary_int oc_name_inx i;
          incr nb_tab;
          Array.set !intSetTab i !nb_tab)
        (NameSort.elements !list_inx);
      close_out oc_name_inx;

      let oc_name_wi = open_out_bin fname_wi in
      let oc_name_w = open_out_bin fname_w in
      let oc_name_i = open_out_bin fname_i in
      let offset_w = ref 0 in
      let offset_i = ref 0 in


      (* avec Set *)
      NameSortMap.iter
        (fun k v ->
          output_binary_int oc_name_wi !offset_w;
          output_binary_int oc_name_w (String.length k);
          output_string oc_name_w k;
          output_binary_int oc_name_w !offset_i;

          offset_w := !offset_w + 4 + (String.length k) + 4;

          (* On tri la liste pour avoir afficher les résultats triés *)
          let vv =
            IntSet.elements
              (List.fold_left
                 (fun accu i -> IntSet.add i accu)
                 IntSet.empty v)
          in
          output_binary_int oc_name_i (List.length vv);
          List.iter (output_binary_int oc_name_i) vv;

          offset_i := !offset_i + 4 + (4 * (List.length vv)))
        !list_map;

      (* avec Hashtbl *)
      (*
      let last_key = ref "" in
      let vv = ref IntSet.empty in
      let len = Hashtbl.length ht in
      let i = ref 0 in
      Hashtbl.iter
        (fun k v ->
          incr i;
          (* On tri la liste pour avoir afficher les résultats triés *)
          if k = !last_key then vv := IntSet.add v !vv else ();

          if k <> !last_key || len = !i then
            begin
              output_binary_int oc_name_wi !offset_w;
              output_binary_int oc_name_w (String.length !last_key);
              output_string oc_name_w !last_key;
              output_binary_int oc_name_w !offset_i;

              offset_w := !offset_w + 4 + (String.length !last_key) + 4;

              output_binary_int oc_name_i (IntSet.cardinal !vv);
              IntSet.iter (output_binary_int oc_name_i) !vv;

              offset_i := !offset_i + 4 + (4 * (IntSet.cardinal !vv));

              vv := IntSet.empty;

            end)
        ht;
      *)

      close_out oc_name_wi;
      close_out oc_name_w;
      close_out oc_name_i;

    with Sys_error _ -> ()
  end
;;


(*
   Fichier ascends index :
    -
*)
let print_ascends_index conf export_directory =
  let bname = Util.base_path conf.bname in
  let fork_base =
    match try Some (Gwdb.open_base bname) with _ -> None with
    | Some base -> base
    | None -> failwith "fork base"
  in
  let base = fork_base in

  let fname_inx = Filename.concat export_directory "pb_base_ascends.inx" in
  match
    try Some (open_out_bin fname_inx)
    with Sys_error _ -> None
  with
  | Some oc ->
      for i = 0 to nb_of_persons base - 1 do
        let ip = Adef.iper_of_int i in
        let p = poi base ip in
        match get_parents p with
        | Some ifam ->
            begin
              let cpl = foi base ifam in
              let father = get_father cpl in
              let mother = get_mother cpl in
              output_char oc '\001';
              output_binary_int oc (Adef.int_of_iper father);
              output_binary_int oc (Adef.int_of_iper mother);
            end
        | None ->
            begin
              output_char oc '\000';
              output_binary_int oc 0;
              output_binary_int oc 0;
            end
      done;
      close_out oc;
  | _ -> ()
;;


let print_export conf base =
  let () = load_ascends_array base in
  let () = load_strings_array base in
  let () = load_couples_array base in
  let () = load_unions_array base in
  let () = load_descends_array base in

  let () = load_image_ht conf in

  (*
     On créé X processus pour l'export :
       - 1/ info
       - 2/ person
       - 3/ family
       - 4/ search
       - 5/ person note
       - 6/ family note
       - 7/ ascends index
  *)

  let export_directory =
    match p_getenvbin conf.env "data" with
    | Some s -> s
    | None -> exit 2
  in

  (* On créé un dossier temporaire pour aller plus vite qu'écrire sur le NAS. *)
  let tmp_export_directory =
    let _ = Random.self_init () in
    let rec loop i =
      let file =
        "/tmp/" ^ conf.bname ^ "." ^ string_of_int (Random.int 1000000) ^ "/"
      in
      if not (Sys.file_exists file) then file
      else if i < 5 then loop (i + 1)
      else exit 2
    in
    loop 0
  in
  let _ =
    try Unix.mkdir tmp_export_directory 0o777
    with Unix.Unix_error (_, _, _) -> exit 2
  in

  let process =
    [ print_index_search; print_export_person; print_export_family;
      print_person_note; print_family_note; print_ascends_index;
      print_export_info ]
  in

  let nb_process = List.length process in

  List.iter
    (fun f ->
       match Unix.fork () with
       | 0 ->
           (* children code *)
           begin
             f conf tmp_export_directory;
             exit 0
           end
       | -1 -> failwith "fork error"
       | _ -> ())
    process;

  (* wait for all children *)
  for i = 0 to nb_process - 1 do ignore (Unix.wait()) done;

  let _ =
    (Sys.command ("mv " ^ tmp_export_directory ^ "/* " ^ export_directory))
  in
  let _ =
    try Unix.rmdir tmp_export_directory with Unix.Unix_error (_, _, _) -> ()
  in


  Util.html conf ;
;;


(**/**) (* Version app, synchro !!! *)


module IntIdSet =
  Set.Make
    (struct
      type t = int;;
      let compare = compare;;
     end)
;;

open Database;;


let full_synchro conf synchro timestamp =
  let last_import = ref None in
  let bdir =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  (* Suppression potentiel du fichier patch. *)
  (match synchro.synch_list with
  | (last_timestamp, _, _) :: _ ->
      let fname_synchro = Filename.concat bdir "synchro_patches" in
      let fname_cmd = Filename.concat bdir "command.txt" in
      (match
         try Some (open_in (Filename.concat (Util.base_path conf.bname) fname_cmd))
         with Sys_error _ -> None
       with
       | Some ic ->
           let fd = Unix.descr_of_in_channel ic in
           let stats = Unix.fstat fd in
           close_in ic;
           last_import := Some stats.Unix.st_mtime;
           if float_of_string last_timestamp < stats.Unix.st_mtime then
             try Sys.remove fname_synchro with Sys_error _ -> ()
           else ()
       | None -> ());
  | _ -> ());
  (* On clean le fichier synchro des trop vieilles modifs. *)
  (match !last_import with
  | Some last_mod ->
      let bname = Util.base_path conf.bname in
      let new_synchro = Database.input_synchro bname in
      let list =
        List.fold_right
          (fun (ts, ipl, ifaml) accu ->
            if (float_of_string ts < last_mod) then accu
            else (ts, ipl, ifaml) :: accu)
          new_synchro.synch_list []
      in
      let new_synchro = {synch_list = list} in
      (* Si on a rien modifier, ça ne sert à rien de faire la mise à *)
      (* jour du fichier synchro, parce qu'on modifie la date de     *)
      (* dernière modification pour rien.                            *)
      if synchro = new_synchro then ()
      else
        begin
          let tmp_fname = Filename.concat bname "1synchro_patches" in
          let fname = Filename.concat bname "synchro_patches" in
          let oc9 =
            try Secure.open_out_bin tmp_fname
            with Sys_error _ ->
              raise (Adef.Request_failure "the database is not writable")
          in
          Mutil.output_value_no_sharing oc9 (synchro : Database.synchro_patch);
          close_out oc9;
          Mutil.remove_file (fname ^ "~");
          (try Sys.rename fname (fname ^ "~") with Sys_error _ -> ());
          (try Sys.rename tmp_fname fname with Sys_error _ -> ());
        end
  | _ -> ());
  (* Si timestamp plus petit que import, alors synchro totale. *)
  match !last_import with
  | Some last_mod -> if timestamp < last_mod then true else false
  | _ -> false
;;


let print_synchro_patch_mobile conf base =
  let params = get_params conf Mext.parse_synchro_params in
  let export_directory = params.M.Synchro_params.export_directory in
  let timestamp = params.M.Synchro_params.timestamp in

  (* On créé un dossier temporaire pour aller plus vite qu'écrire sur le NAS. *)
  let tmp_export_directory =
    let _ = Random.self_init () in
    let rec loop i =
      let file =
        "/tmp/" ^ conf.bname ^ "." ^ string_of_int (Random.int 1000000) ^ "/"
      in
      if not (Sys.file_exists file) then file
      else if i < 5 then loop (i + 1)
      else exit 2
    in
    loop 0
  in
  let _ =
    try Unix.mkdir tmp_export_directory 0o777
    with Unix.Unix_error (_, _, _) -> exit 2
  in

  (* Récupération du fichier synchro. *)
  let bname = Util.base_path conf.bname in
  let synchro = Database.input_synchro bname in
  (* Toutes les dernières modifications. *)
  let timestamp = float_of_string timestamp in
  let (ip_list, ifam_list) =
    List.fold_right
      (fun (t_stamp, ip_list, ifam_list) (accu_ip_list, accu_ifam_list) ->
        let t_stamp = float_of_string t_stamp in
        if t_stamp > timestamp then
          (accu_ip_list @ ip_list, accu_ifam_list @ ifam_list)
        else (accu_ip_list, accu_ifam_list))
      synchro.synch_list ([], [])
  in
  let last_timestamp =
    match synchro.synch_list with
    | (timestamp, _, _) :: _ -> timestamp
    | _ -> ""
  in
  (* On rend unique les ids. *)
  let ip_list =
    IntIdSet.elements
      (List.fold_left
         (fun accu i -> IntIdSet.add i accu)
         IntIdSet.empty ip_list)
  in
  let ifam_list =
    IntIdSet.elements
      (List.fold_left
         (fun accu i -> IntIdSet.add i accu)
         IntIdSet.empty ifam_list)
  in
  let len_ip_list = List.length ip_list in
  let len_ifam_list = List.length ifam_list in

  (* Ecriture du fichier synchro. *)
  let fname = Filename.concat tmp_export_directory "pb_base_synchro.patches" in
  let () =
    match
      try Some (open_out_bin fname)
      with Sys_error _ -> None
    with
    | Some oc ->
        if full_synchro conf synchro timestamp then
          (* si 0 il faut re-synchroniser la base. *)
          output_char oc '\000'
        else
          begin
            (* si 1 il faut appliquer le patch. *)
            output_char oc '\001';
            output_binary_int oc (String.length last_timestamp);
            output_string oc last_timestamp;
            (* nb persons et families *)
            output_binary_int oc (Util.real_nb_of_persons conf base);
            output_binary_int oc (nb_of_families base);
            (* sosa *)
            let sosa_ref =
              match Util.find_sosa_ref conf base with
              | Some p ->
                  (output_char oc '\001'; Adef.int_of_iper (get_key_index p))
              | None -> (output_char oc '\000'; 0)
            in
            output_binary_int oc sosa_ref;
            (* nb pers modified, id len pers *)
            output_binary_int oc len_ip_list;
            List.iter
              (fun i ->
                let ip = Adef.iper_of_int i in
                let p = poi base ip in
                let pers_app = pers_to_piqi_app_person conf base p in
                let data = Mext_app.gen_person pers_app in
                let data = data `pb in
                (* id, longueur de la personne puis données de la personne *)
                output_binary_int oc i;
                output_binary_int oc (String.length data);
                output_string oc data)
              ip_list;
            (* nb fam modified, id len fam *)
            output_binary_int oc len_ifam_list;
            List.iter
              (fun i ->
                let ifam = Adef.ifam_of_int i in
                let fam_app = fam_to_piqi_app_family base ifam in
                let data = Mext_app.gen_family fam_app in
                let data = data `pb in
                (* id, longueur de la famille puis données de la famille *)
                output_binary_int oc i;
                output_binary_int oc (String.length data);
                output_string oc data)
              ifam_list;
            (* nb pers modified, id len pers_note *)
            output_binary_int oc len_ip_list;
            List.iter
              (fun i ->
                let ip = Adef.iper_of_int i in
                let p = poi base ip in
                let data = sou base (get_notes p) in
                if data = "" then
                  begin
                    (* On pointe vers la note vide. *)
                    output_binary_int oc i;
                    output_binary_int oc 0
                  end
                else
                  begin
                    (* id, longueur de la personne puis données de la personne *)
                    output_binary_int oc i;
                    output_binary_int oc (String.length data);
                    output_string oc data
                  end)
              ip_list;
            (* nb fam modified, id len fam_note *)
            output_binary_int oc len_ifam_list;
            List.iter
              (fun i ->
                let ifam = Adef.ifam_of_int i in
                let fam = foi base ifam in
                let data = sou base (get_comment fam) in
                if data = "" then
                  begin
                    (* On pointe vers la note vide. *)
                    output_binary_int oc i;
                    output_binary_int oc 0
                  end
                else
                  begin
                    (* id, longueur de la personne puis données de la personne *)
                    output_binary_int oc i;
                    output_binary_int oc (String.length data);
                    output_string oc data
                  end)
              ifam_list;
            (* nb pers modified, id has_parents id_father id_mother *)
            output_binary_int oc len_ip_list;
            List.iter
              (fun i ->
                let ip = Adef.iper_of_int i in
                let p = poi base ip in
                output_binary_int oc i;
                match get_parents p with
                | Some ifam ->
                    begin
                      let cpl = foi base ifam in
                      let father = get_father cpl in
                      let mother = get_mother cpl in
                      output_char oc '\001';
                      output_binary_int oc (Adef.int_of_iper father);
                      output_binary_int oc (Adef.int_of_iper mother);
                    end
                | None ->
                    begin
                      output_char oc '\000';
                      output_binary_int oc 0;
                      output_binary_int oc 0;
                    end)
              ip_list;
            (* number of character => to be modified
               when we actually know the number. *)
            let nb_char = ref 0 in
            let pos_nb_char = pos_out oc in
            output_binary_int oc !nb_char;
            (* id nb_word len_word word *)
            List.iter
              (fun i ->
                let ip = Adef.iper_of_int i in
                let p = poi base ip in
                let fn = sou base (get_first_name p) in
                let sn = sou base (get_surname p) in
                if sn = "?" && fn = "?" then ()
                else
                  begin
                    let fn = Name.lower fn in
                    let sn = Name.lower sn in
                    let r = build_relative_name base p in
                    output_binary_int oc i;
                    let (split_l, nb_words, nb_chars) =
                      List.fold_left
                        (fun (split_l, nb_words, nb_chars) s ->
                           (* FIXME: Does order matter or not? *)
                           let l = String.split_on_char ' ' s |> List.rev in
                           let sub_nb_chars =
                             match List.length l with
                             | 0 -> 0
                             | x -> String.length s - (x - 1)
                           in
                          (List.rev_append split_l l,
                           nb_words + List.length l,
                           nb_chars + sub_nb_chars))
                        ([], 0, 0) (sn :: fn :: r)
                    in
                    nb_char := 4 + 4 + (4 * nb_words) + nb_chars + !nb_char;
                    output_binary_int oc nb_words;
                    List.iter
                      (fun s ->
                        output_binary_int oc (String.length s);
                        output_string oc s)
                      split_l
                  end)
              ip_list;
            (* update nb_char *)
            seek_out oc pos_nb_char;
            output_binary_int oc !nb_char;
          end;
        close_out oc;
    | _ -> exit 2
  in

  (* move file *)
  let _ =
    (Sys.command ("mv " ^ tmp_export_directory ^ "/* " ^ export_directory))
  in
  let _ =
    try Unix.rmdir tmp_export_directory with Unix.Unix_error (_, _, _) -> ()
  in

  Util.html conf ;
;;


let print_export_search conf base =
  let () = load_strings_array base in

  let export_directory =
    match p_getenvbin conf.env "data" with
    | Some s -> s
    | None -> exit 2
  in

  (* On créé un dossier temporaire pour aller plus vite qu'écrire sur le NAS. *)
  let tmp_export_directory =
    let _ = Random.self_init () in
    let rec loop i =
      let file =
        "/tmp/" ^ conf.bname ^ "." ^ string_of_int (Random.int 1000000) ^ "/"
      in
      if not (Sys.file_exists file) then file
      else if i < 5 then loop (i + 1)
      else exit 2
    in
    loop 0
  in
  let _ =
    try Unix.mkdir tmp_export_directory 0o777
    with Unix.Unix_error (_, _, _) -> exit 2
  in

  (* Génération des fichiers de recherche. *)
  print_index_search conf tmp_export_directory;

  (* move file *)
  let _ =
    (Sys.command ("mv " ^ tmp_export_directory ^ "/* " ^ export_directory))
  in
  let _ =
    try Unix.rmdir tmp_export_directory with Unix.Unix_error (_, _, _) -> ()
  in

  Util.html conf ;
;;


(*
let print_export conf base =
  let () = load_ascends_array base in
  let () = load_strings_array base in
  let () = load_couples_array base in
  let () = load_unions_array base in
  let () = load_descends_array base in

  let finfo = "/tmp/pb_base_info.dat" in
  let fpers_data = "/tmp/pb_base_person.dat" in
  let fpers_inx = "/tmp/pb_base_person.inx" in
  let ffam_data = "/tmp/pb_base_family.dat" in
  let ffam_inx = "/tmp/pb_base_family.inx" in

  let pid = Unix.fork () in

  if pid = 0 then
    begin
      print_index_search conf base;
      exit 0
    end
  else
  begin
    (try
      let curr = ref 0 in

      (*
         Fichier base_info :
           - nombre de personnes
           - nombre de familles
           - sosa de référence (1-num s'il existe, 0-0 sinon)
           - timestamp de la création de la base
      *)
      let oc_info = open_out_bin finfo in
      output_binary_int oc_info (nb_of_persons base);
      output_binary_int oc_info (nb_of_families base);
      let sosa_ref =
        match Util.find_sosa_ref conf base with
        | Some p ->
            (output_char oc_info '\001'; Adef.int_of_iper (get_key_index p))
        | None -> (output_char oc_info '\000'; 0)
      in
      output_binary_int oc_info sosa_ref;
      let timestamp = int_of_float (Unix.time ()) in
      output_binary_int oc_info timestamp;
      close_out oc_info;

      (*
         Fichier person index :
           - l'adresse dans le fichier data de cette personne
         Fichier person data :
           - offset delete : à la création, c'est la fin du fichier
           - liste des Person (proto app)
      *)
      let oc_pers_data = open_out_bin fpers_data in
      let oc_pers_inx = open_out_bin fpers_inx in
      curr := 0;
      (* offset delete *)
      output_binary_int oc_pers_data 0;
      for i = 0 to nb_of_persons base - 1 do
        let ip = Adef.iper_of_int i in
        let p = poi base ip in
        let pers_app = pers_to_piqi_app_person conf base p in
        let data = Mext_app.gen_person pers_app in
        let data = data `pb in
        output_binary_int oc_pers_data (String.length data);
        output_string oc_pers_data data;
        (* Adresse de la personne *)
        output_binary_int oc_pers_inx !curr;
        (* Attention a ne pas oublier offset delete => +4 *)
        curr := !curr + 4 + String.length data;
      done;
      (* mise à jour de offset delete maintenant qu'on a fini *)
      seek_out oc_pers_data 0;
      output_binary_int oc_pers_data !curr;
      close_out oc_pers_data;
      close_out oc_pers_inx;

      (*
         Fichier family index :
           - l'adresse dans le fichier data de cette famille
         Fichier family data :
           - offset delete : à la création, c'est la fin du fichier
           - liste des Family
      *)
      let oc_fam_data = open_out_bin ffam_data in
      let oc_fam_inx = open_out_bin ffam_inx in
      curr := 0;
      (* offset delete *)
      output_binary_int oc_fam_data 0;
      for i = 0 to nb_of_families base - 1 do
        let ifam = Adef.ifam_of_int i in
        let fam_app = fam_to_piqi_app_family conf base ifam in
        let data = Mext_app.gen_family fam_app in
        let data = data `pb in
        output_binary_int oc_fam_data (String.length data);
        output_string oc_fam_data data;
        (* Adresse de la famille *)
        output_binary_int oc_fam_inx !curr;
        (* Attention a ne pas oublier offset delete => +4 *)
        curr := !curr + 4 + String.length data;
      done;
      (* mise à jour de offset delete maintenant qu'on a fini *)
      seek_out oc_fam_data 0;
      output_binary_int oc_fam_data !curr;
      close_out oc_fam_data;
      close_out oc_fam_inx;

    with Sys_error _ -> ());
    ignore (Unix.waitpid [] pid);
  end;

  (*export_img conf base;*)

(*
  let fpers_data = "/tmp/pb_base_person_note.dat" in
  let fpers_inx = "/tmp/pb_base_person_note.inx" in
  let ffam_data = "/tmp/pb_base_family_note.dat" in
  let ffam_inx = "/tmp/pb_base_family_note.inx" in

  (try
     let curr = ref 0 in

     (*
        Fichier pers_note index :
          - l'adresse dans le fichier data de cette note
        Fichier pers_note data :
          - liste des notes
     *)
     let oc_pers_data = open_out_bin fpers_data in
     let oc_pers_inx = open_out_bin fpers_inx in
     for i = 0 to nb_persons base - 1 do
       let ip = Adef.iper_of_int i in
       let p = poi base ip in
       let data = sou base (get_notes p) in
       if data <> "" then
         begin
           output_binary_int oc_pers_data (String.length data);
           output_string oc_pers_data notes;
           ouput_binary_int oc_pers_inx !curr;
           curr := !curr + String.length data;
         end
     done;
     close_out oc_pers_data;
     close_out oc_pers_inx;

     (*
        Fichier fam_note index :
          - l'adresse dans le fichier data de cette note
        Fichier fam_note data :
          - liste des notes
     *)
     let oc_fam_data = open_out_bin ffam_data in
     let oc_fam_inx = open_out_bin ffam_inx in
     curr := 0;
     for i = 0 to nb_persons base - 1 do
       let ifam = Adef.ifam_of_int i in
       let fam = foi base ifam in
       let data = sou base (get_comment fam) in
       if data <> "" then
         begin
           output_binary_int oc_fam_data (String.length data);
           output_string oc_fam_data notes;
           ouput_binary_int oc_fam_inx !curr;
           curr := !curr + String.length data;
         end
     done;
     close_out oc_fam_data;
     close_out oc_fam_inx;
   with Sys_error _ -> ());
*)


  Util.html conf ;
;;
*)



(**/**) (* API_NOTIFICATION_BIRTHDAY *)


(* !!! Repris de api_graph !!! *)

module Iper3 =
  struct
    type t = (Adef.iper * int * M.relation_type)
    let compare (i1, _, _) (i2, _, _) =
      Pervasives.compare (Adef.int_of_iper i1) (Adef.int_of_iper i2)
  end

module IperSet3 = Set.Make(Iper3) ;;



(* ******************************************************************** *)
(*  [Fonc] print_notification_birthday :
             config -> base -> NotificationBirthday                     *)
(** [Description] : Retourne le nombre d'anniversaires pour les
                    notifications.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - NotificationBirthday : Le nombre d'anniversaire ainsi que les
          trois premiers prénoms pour les anniversaires.
    [Rem] : Non exporté en clair hors de ce module.                     *)
(* ******************************************************************** *)
let print_notification_birthday conf base =
  let params = get_params conf Mext.parse_notification_birthday_params in
  let ref_p = params.M.Notification_birthday_params.person in
  let (nb_asc, nb_desc, nb_asc_sp) =
    match params.M.Notification_birthday_params.params with
    | `close_person -> (2, -2, 1)
    | `descend_grand_parent -> (2, -3, 2)
    | `descend_great_grand_parent -> (3, -4, 3)
  in
  let ip_proprio =
    match piqi_ref_person_to_person base ref_p with
    | Some p -> get_key_index p
    | None -> Adef.iper_of_int (-1)
  in
  let p = poi base ip_proprio in
  let list =
    Api_graph.close_person_relation
      conf base ip_proprio nb_asc nb_desc false
  in
  let list =
    List.fold_left
      (fun accu ifam ->
        let fam = foi base ifam in
        let isp = Gutil.spouse ip_proprio fam in
        let sp_list =
          Api_graph.close_person_relation
            conf base isp nb_asc_sp nb_desc false
        in
        List.rev_append accu sp_list)
      list (Array.to_list (get_family p))
  in
  (* Rendre unique la liste. Un peu crado,
     reprends le type IperSet de api_graph *)
  let list =
    IperSet3.elements
      (List.fold_left
         (fun accu elt -> IperSet3.add elt accu)
         IperSet3.empty list)
  in
  (* On filtre la liste par rapport aux anniversaires. *)
  let list =
    match
      (params.M.Notification_birthday_params.month,
       params.M.Notification_birthday_params.day)
    with
    | (Some month, Some day) ->
        let (month, day) = (Int32.to_int month, Int32.to_int day) in
        (* Anniversaire du jour. *)
        List.fold_left
          (fun accu (ip, _, _) ->
            let p = poi base ip in
            match Adef.od_of_cdate (get_birth p) with
            | Some (Dgreg (d, _)) ->
                if d.prec = Sure && get_death p = NotDead &&
                   d.day = day && d.month = month
                then (ip :: accu)
                else accu
            | _ -> accu)
          [] (List.rev list)
    | (Some month, None) ->
        let month = (Int32.to_int month) in
        (* Anniversaire du mois. *)
        List.fold_left
          (fun accu (ip, _, _) ->
            let p = poi base ip in
            match Adef.od_of_cdate (get_birth p) with
            | Some (Dgreg (d, _)) ->
                if d.prec = Sure && get_death p = NotDead && d.month = month
                then (ip :: accu)
                else accu
            | _ -> accu)
          [] (List.rev list)
    | _ -> List.map (fun (ip, _, _) -> ip) list
  in
  (* On filtre le proprio de la liste. *)
  let (list, has_proprio_birthday) =
    List.fold_left
      (fun (accu, has_birthday) ip ->
         if ip = ip_proprio then (accu, true) else (ip :: accu, has_birthday))
      ([], false) (List.rev list)
  in
  let (fn1, fn2, fn3) =
    match list with
    | [] -> (None, None, None)
    | [ip1] ->
        let p1 = poi base ip1 in
        (Some (sou base (get_first_name p1)), None, None)
    | [ip1; ip2] ->
        let p1 = poi base ip1 in
        let p2 = poi base ip2 in
        (Some (sou base (get_first_name p1)),
         Some (sou base (get_first_name p2)), None)
    | ip1 :: ip2 :: ip3 :: _ ->
        let p1 = poi base ip1 in
        let p2 = poi base ip2 in
        let p3 = poi base ip3 in
        (Some (sou base (get_first_name p1)),
         Some (sou base (get_first_name p2)),
         Some (sou base (get_first_name p3)))
  in
  let notification_birthday =
    M.Notification_birthday.({
      number = Int32.of_int (List.length list);
      has_proprio_birthday = has_proprio_birthday;
      firstname1 = fn1;
      firstname2 = fn2;
      firstname3 = fn3;
    })
  in
  let data = Mext.gen_notification_birthday notification_birthday in
  print_result conf data

#endif

