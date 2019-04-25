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
  let last_modified_person =
    let default () = Opt.map (fun p -> Gwdb.string_of_iper (get_key_index p)) sosa_p in
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
               | "mn" -> default ()
               | _ ->
                 (match Gutil.person_ht_find_all base key with
                  | [ip] -> Some (Gwdb.string_of_iper ip)
                  | _ -> default ()))
            | None -> default ()
          else default ()
        | None -> default ()
      in
      close_in ic;
      last_modified_person
    with Sys_error _ | _ -> default ()
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
    (ref false, ref (poi base (Gwdb.dummy_iper)))
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
  let ip = Gwdb.iper_of_string id.M.Index.index in
  let p = poi base ip in
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
  let continue = ref true in
  let res = ref empty_ref in
  Gwdb.Collection.fold_until (fun () -> !continue) begin fun () p ->
    if is_hide_names conf p || is_empty_or_quest_name p ||
       not (authorized_age conf base p)
    then ()
    else
      begin
        let fn = Name.lower (sou base (get_first_name p)) in
        let sn = Name.lower (sou base (get_surname p)) in
        let occ = Int32.of_int (get_occ p) in
        res :=
          M.Reference_person.({
              n = sn;
              p = fn;
              oc = occ;
            }) ;
        continue := false
      end
  end () (Gwdb.persons base) ;
  let data = Mext.gen_reference_person !res in
  print_result conf data

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
  let date_before_interval time =
    match range with
    | Some (date_begin, _, prec) ->
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
      let dmy_zero = { day = 1; month = 1; year = 1970; prec = Sure; delta = 0; } in
      is_date_included prec date dmy_zero date_begin
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
  let list =
    match
      try Some (Secure.open_in_bin (History.file_name conf))
      with Sys_error _ -> None
    with
    | Some ic ->
        let () = Perso.build_sosa_ht conf base in
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
                    if (wiz = "" || user = wiz) then
                      if is_time_included time then
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
                                        conf filters (Perso.get_sosa_person) p &&
                                      not (p_mem ip list)
                                    then loop (p :: list) (res - 1) pos
                                    else loop list res pos
                                | _ -> loop list res pos))
                        | None -> loop list res pos
                      else
                        if date_before_interval time then list
                        else loop list res pos
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
  let module IperSet = Set.Make (struct type t = iper let compare = compare end) in

  let ipers = Gwdb.ipers base in
  let ancestors = Gwdb.iper_marker ipers IperSet.empty in
  let mark = Gwdb.iper_marker ipers false in

  let has_children p =
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
    if Gwdb.Marker.get mark ip then Gwdb.Marker.get ancestors ip
    else
      begin
        let anc =
          match get_parents (poi base ip) with
          | Some ifam ->
              let cpl = foi base ifam in
              let anc =
                IperSet.add (get_father cpl) @@ Gwdb.Marker.get ancestors ip
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
        Gwdb.Marker.set ancestors ip anc;
        Gwdb.Marker.set mark ip true;
        anc
      end
  in

  Gwdb.Collection.iter begin fun p ->
    if has_children p || Gwdb.Marker.get mark (get_key_index p) then ()
    else
      begin
        let i = get_key_index p in
        let anc = nb_ancestors i in
        Gwdb.Marker.set ancestors i anc;
        Gwdb.Marker.set mark i true
      end
  end (Gwdb.persons base) ;

  (* ip, nb_anc *)
  let res = ref (Gwdb.dummy_iper, 0) in
  Gwdb.Collection.iter begin fun i ->
    let nb = IperSet.cardinal @@ Gwdb.Marker.get ancestors i in
    if nb > snd !res then res := (i, nb)
  end ipers ;

  let p = poi base (fst !res) in
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


(**/**) (* API_IMAGE *)

let print_img conf base =
  let filters = get_filters conf in
  let aux fp fl =
    let () = Perso.build_sosa_ht conf base in
    let () = load_image_ht conf in
    let list =
      Gwdb.Collection.fold begin fun acc p ->
        match Api_util.find_image_file conf base p with
        | Some img -> fp p img :: acc
        | None -> acc
      end [] (Gwdb.persons base)
    in
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
      let data = Mext.gen_internal_int32 len in
      print_result conf data
    else
      print_result conf (fl list)
  in
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    aux
      (fun p img ->
         let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true in
         M.Full_image.({person = p; img }))
      (fun list ->
         Mext.gen_list_full_images @@ M.List_full_images.({images = list}) )
  else
    aux
      (fun p img ->
         let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person true in
         M.Image.({person = p; img}))
      (fun list ->
         Mext.gen_list_images @@ M.List_images.({list_images = list}) )

(**/**) (* API_IMAGE_EXT *)

let print_img_ext conf base =
  let filters = get_filters conf in
  let aux fp fl =
    let () = Perso.build_sosa_ht conf base in
    let () = load_image_ht conf in
    let list =
      Gwdb.Collection.fold begin fun acc p ->
        let http = "http://" in
        let img = sou base (get_image p) in
        if not (is_empty_string (get_image p)) &&
           String.length img > String.length http &&
           String.sub img 0 (String.length http) = http
        then
          fp p img :: acc
        else acc
      end [] (Gwdb.persons base)
    in
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
      let data = Mext.gen_internal_int32 len in
      print_result conf data
    else print_result conf (fl list)
  in
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    aux
      (fun p img ->
         let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person true in
         M.Full_image.({person = p; img = img;}))
      (fun list ->
         Mext.gen_list_full_images @@ M.List_full_images.({images = list}) )
  else
    aux
      (fun p img ->
         let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person true in
         M.Image.({person = p; img = img;}))
      (fun list -> Mext.gen_list_images @@ M.List_images.({list_images = list}))

(**/**) (* API_IMAGE_ALL *)

let print_img_all conf base =
  let filters = get_filters conf in
  let aux fp fl =
    let list =
      Gwdb.Collection.fold begin fun acc p ->
        if not (is_empty_string (get_image p)) then
          let img = sou base (get_image p) in
          fp p img :: acc
        else
          match Api_util.find_image_file conf base p with
          | Some img -> fp p img :: acc
          | None -> acc
      end [] (Gwdb.persons base)
    in
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length list)}) in
      let data = Mext.gen_internal_int32 len in
      print_result conf data
    else
      print_result conf (fl list)
  in
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    aux
      (fun p img ->
         let p = pers_to_piqi_person_full conf base p base_loop Perso.get_sosa_person false in
         M.Full_image.({person = p; img = img;}))
      (fun list -> Mext.gen_list_full_images @@ M.List_full_images.({images = list}) )
  else
    aux
      (fun p img ->
         let p = pers_to_piqi_person_light conf base p base_loop Perso.get_sosa_person false in
         M.Image.({person = p; img}))
      (fun list ->
         Mext.gen_list_images @@ M.List_images.({list_images = list}))

(**/**) (* API_IMAGE_APP *)

let print_img_person conf base =
  let id = get_params conf Mext.parse_index in
  let ip = Gwdb.iper_of_string id.M.Index.index in
  let p = poi base ip in
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
  Gwdb.commit_patches base

(**/**) (* API_REMOVE_IMAGE_EXT *)

let print_remove_image_ext base =
  Gwdb.Collection.iter begin fun p ->
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
  end (Gwdb.persons base) ;
  Gwdb.commit_patches base

(**/**) (* API_REMOVE_IMAGE_EXT_ALL *)

let print_remove_image_ext_all base =
  Gwdb.Collection.iter begin fun p ->
    if not (is_empty_string (get_image p)) then
      let p =
        {(gen_person_of_person p) with image = Gwdb.insert_string base ""}
      in
      patch_person base p.key_index p
  end (Gwdb.persons base) ;
  Gwdb.commit_patches base

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
  let next = Gwdb.Collection.iterator (Gwdb.ipers base) in
  fun () -> match next () with
    | Some i -> pget conf base i
    | None -> raise Not_found

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
  let len = limit - from in
  let list =
    Gwdb.Collection.fold_until
      (fun (_, n) -> n < len)
      begin fun ((list, n) as acc) i ->
        if n < from then acc
        else (i :: list, n + 1)
      end ([], 0) (Gwdb.ipers base)
  in
  let list = Gwdb.poi_batch base (List.rev @@ fst list) in
  let data = data_list_person conf base filters list in
  print_result conf data

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
  let () = Perso.build_sosa_ht conf base in
  let len = limit - from in
  let list =
    Gwdb.Collection.fold_until
      (fun (_, n) -> n < len)
      begin fun ((list, n) as acc) i ->
        if n < from then acc
        else (i :: list, n + 1)
      end ([], 0) (Gwdb.ifams base)
  in
  let data =
    if filters.nb_results then
      let len = M.Internal_int32.({value = Int32.of_int (List.length @@ fst list)}) in
      Mext.gen_internal_int32 len
    else
      let list = List.map (fam_to_piqi_family conf base) (List.rev @@ fst list) in
      let list = M.List_full_families.({families = list}) in
      Mext.gen_list_full_families list
  in
  print_result conf data

(**/**) (* API_NOTIFICATION_BIRTHDAY *)

module Iper3 =
  struct
    type t = (Gwdb.iper * int * M.relation_type)
    let compare (i1, _, _) (i2, _, _) =
      Pervasives.compare (i1) (i2)
  end

module IperSet3 = Set.Make(Iper3)


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
    | None -> Gwdb.dummy_iper
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
