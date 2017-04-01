(* nocamlp5 *)


module Mread = Api_saisie_read_piqi
module Mext_read = Api_saisie_read_piqi_ext


module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext


open Config
open Def
open Gwdb
open Util
open Api_def
open Api_util



(**/**) (* Conversion de dates *)

(* Copie de date.ml sans les balises HTML => on devrait créer *)
(* un date_api.ml qu'on utiliserait à la place de date.ml     *)

let short_prec_year_text conf d =
  let prec =
    match d.prec with
    | About | OrYear _ | YearInt _| YearDur _ ->
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        (match transl conf "about (short date)" with
         | "ca" -> "ca "
         | s -> s ^ " ")
    | Maybe -> "? "
    | Before -> "< "
    | After -> "> "
    | _ -> ""
  in
  prec ^ string_of_int d.year
;;


let partial_short_dates_text conf birth_date death_date p =
  match (birth_date, death_date) with
  | (Some (Dgreg (b, _)), Some (Dtext _)) -> short_prec_year_text conf b ^ "-"
  | (Some (Dgreg (b, _)), None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
          short_prec_year_text conf b ^ "-"
      | _ -> short_prec_year_text conf b )
  | (None, Some (Dtext _)) ->
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> Date.death_symbol conf
      | _ -> "" )
  | (None, None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> Date.death_symbol conf
      | _ -> "" )
  | (_, _) -> ""
;;

let short_dates_text conf base p =
  if authorized_age conf base p then
    let (birth_date, death_date, _) = Date.get_birth_death_date p in
    let s =
      match (birth_date, death_date) with
      | (Some (Dgreg (b, _)), Some (Dgreg (d, _))) ->
          short_prec_year_text conf b ^ "-" ^ short_prec_year_text conf d
      | (Some (Dgreg (b, _)), None) ->
          (* La personne peut être décédée mais ne pas avoir de date. *)
          (match get_death p with
          | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
              short_prec_year_text conf b ^ "-"
          | _ -> short_prec_year_text conf b )
      | (None, Some (Dgreg (d, _))) ->
          (match get_death p with
          | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
              Date.death_symbol conf ^ short_prec_year_text conf d
          | _ -> "" )
      | (None, None) ->
          (* La personne peut être décédée mais ne pas avoir de date. *)
          (match get_death p with
          | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
              Date.death_symbol conf
          | _ -> "" )
      (* On ne peut pas traiter les dates au format texte, mais on *)
      (* affiche tout de même les dates au format Dgreg.           *)
      | (_, _) -> partial_short_dates_text conf birth_date death_date p
    in
    s
  else ""
;;

let code_french_date conf d m y =
  let s =
    if d = 0 then ""
    else string_of_int d
  in
  let s =
    if m = 0 then ""
    else s ^ (if s = "" then "" else " ") ^ Date.french_month conf (m - 1)
  in
  s ^ (if s = "" then "" else " ") ^ Date.code_french_year conf y
;;

let string_of_french_dmy conf d =
  code_french_date conf d.day d.month d.year
;;

let string_of_hebrew_dmy conf d =
  Date.code_hebrew_date conf d.day d.month d.year
;;

let encode_dmy conf d m y =
  let date = if d != 0 then string_of_int d else "" in
  let date =
    if m != 0 then
      if date = "" then transl_nth conf "(short month)" (m - 1)
      else date ^ " " ^ transl_nth conf "(short month)" (m - 1)
    else date
  in
  let date =
    if date = "" then string_of_int y
    else date ^ " " ^ string_of_int y
  in
  date
;;

let string_of_dmy conf d =
  let sy = encode_dmy conf d.day d.month d.year in
  let sy2 =
    match d.prec with
    | OrYear d2 | YearInt d2 | YearDur d2 ->
        let d2 = Date.dmy_of_dmy2 d2 in
        encode_dmy conf d2.day d2.month d2.year
    | _ -> ""
  in
  Date.string_of_prec_dmy conf sy sy2 d
;;

let gregorian_precision conf d =
  if d.delta = 0 then string_of_dmy conf d
  else
    let d2 =
      Calendar.gregorian_of_sdn d.prec (Calendar.sdn_of_gregorian d + d.delta)
    in
    transl conf "between (date)" ^ " " ^ string_of_dmy conf d ^ " " ^
      transl_nth conf "and" 0 ^ " " ^ string_of_dmy conf d2
;;


(* ************************************************************************** *)
(*  [Fonc] string_of_date_and_conv :
             config -> Def.date -> (string * string * cal)                    *)
(** [Description] : Renvoie la date, la date traduite et le calendrier au
                    format texte.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - d         : date
    [Retour] :
      - (date greg * date * calendar option)
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let string_of_date_and_conv conf d =
  match d with
  | Dgreg (d, Dgregorian) ->
      (string_of_dmy conf d, "", Some `gregorian)
  | Dgreg (d, Djulian) ->
      let date_conv =
        if d.year < 1582 then "" else gregorian_precision conf d
      in
      let d1 = Calendar.julian_of_gregorian d in
      let year_prec =
        if d1.month > 0 && d1.month < 3 ||
           d1.month = 3 && d1.day > 0 && d1.day < 25 then
          Printf.sprintf " (%d/%d)" (d1.year - 1) (d1.year mod 10)
        else ""
      in
      let date =
        Date.string_of_dmy conf d1 ^ year_prec ^ " " ^
          transl_nth conf "gregorian/julian/french/hebrew" 1
      in
      (date_conv, date, Some `julian)
  | Dgreg (d, Dfrench) ->
      let d1 = Calendar.french_of_gregorian d in
      let date = string_of_french_dmy conf d1 in
      (match d.prec with
      | Sure -> (gregorian_precision conf d, date, Some `french)
      | About | Before | After | Maybe | OrYear _ | YearInt _ | YearDur _ ->
          (date, "", Some `french))
  | Dgreg (d, Dhebrew) ->
      let d1 = Calendar.hebrew_of_gregorian d in
      let date = string_of_hebrew_dmy conf d1 in
      (match d.prec with
      | Sure -> (gregorian_precision conf d, date, Some `hebrew)
      | About | Before | After | Maybe | OrYear _ | YearInt _ | YearDur _ ->
          (date, "", Some `hebrew))
  | Dtext t -> ("(" ^ string_with_macros conf [] t ^ ")", "", None)
;;


(**/**) (* Affichage nom/prénom *)

let person_firstname_surname_txt conf base p =
  if not (is_empty_string (get_public_name p)) then
    let fn = sou base (get_public_name p) in
    let sn =
      match get_qualifiers p with
      | s :: l -> " " ^ sou base s
      | _ -> sou base (get_surname p)
    in
    (fn, sn)
  else
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let sn =
      match get_qualifiers p with
      | s :: l -> sn ^ " " ^ sou base s
      | _ -> sn
    in
    (fn, sn)
;;


(**/**) (* Fonctions de transformation person <=> piqi person *)


type graph_more_info =
  | Root
  | Siblings
  | Children
  | Ancestor
  | Spouse
;;


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_tree : config -> base -> person -> PersonTree  *)
(** [Description] : Retourne à partir d'une person (gwdb) une PersonTree (piqi)
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix =
  if is_restricted conf base (get_key_index p) then
    Mread.Person_tree.({
      index = Int32.of_int (-1);
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      dates = None;
      image = None;
      sosa = `no_sosa;
      has_more_infos = false;
      baseprefix = "";
    })
  else
    let p_auth = authorized_age conf base p in
    let index = Int32.of_int (Adef.int_of_iper (get_key_index p)) in
    let sex =
      match get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      if conf.bname <> Link.chop_base_prefix base_prefix then `no_sosa
      else
        let sosa_nb = Perso.get_sosa_person conf base p in
        if Sosa.eq sosa_nb Sosa.zero then `no_sosa
        else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
        else `sosa
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_surname p))
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_first_name p))
    in
    let occ = Int32.of_int (get_occ p) in
    let (first_name, surname) =
      if not p_auth && (is_hide_names conf p) then ("x", "x")
      else person_firstname_surname_txt conf base p
    in
    let dates = short_dates_text conf base p in
    let image =
      if not conf.no_image && p_auth then
        let img = sou base (get_image p) in
        if img <> "" then img
        else
          match Api_util.find_image_file conf base p with
          | Some s -> "1"
          | None -> ""
      else ""
    in
    let has_more_infos =
      match more_info with
      | Root -> false
      | Siblings -> Array.length (get_family p) > 0
      | Children ->
           gen = max_gen - 1 && Array.length (get_family p) > 0
           (*
           fst (List.fold_left
                  (fun (children_or_spouses, nb_fam) ifam ->
                    let nb_fam = succ nb_fam in
                    let fam = foi base ifam in
                    let children = get_children fam in
                    (children_or_spouses || Array.length children > 1 ||
                       nb_fam > 1, nb_fam))
                  (false, 0) (Array.to_list (get_family p)))
           *)
      | Ancestor ->
          let has_parents =
            match get_parents p with
            | Some ifam -> true
            | _ -> false
          in
          (gen = max_gen - 1 && has_parents) ||
           (fst (List.fold_left
                   (fun (children_or_spouses, nb_fam) ifam ->
                     let nb_fam = succ nb_fam in
                     let fam = foi base ifam in
                     let children = get_children fam in
                     (children_or_spouses || (gen > 1 && Array.length children > 1) || nb_fam > 1,
                      nb_fam))
                   (false, 0) (Array.to_list (get_family p))))
      | Spouse ->
          let has_parents =
            match get_parents p with
            | Some ifam -> true
            | _ -> false
          in
          has_parents || Array.length (get_family p) > 1
    in
    Mread.Person_tree.({
      index = index;
      sex = sex;
      lastname = surname;
      firstname = first_name;
      n = sn;
      p = fn;
      occ = occ;
      dates = if dates = "" then None else Some dates;
      image = if image = "" then None else Some image;
      sosa = sosa;
      has_more_infos = has_more_infos;
      baseprefix = base_prefix
    })
;;


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_simple_person :
             config -> base -> person -> SimplePerson                         *)
(** [Description] : Retourne à partir d'une person (gwdb) une SimplePerson
                    (piqi).
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_simple_person conf base p base_prefix =
  if is_restricted conf base (get_key_index p) then
    Mread.Simple_person.({
      index = Int32.of_int (-1);
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      birth_short_date = None;
      birth_place = None;
      death_short_date = None;
      death_place = None;
      image = None;
      sosa = `no_sosa;
      baseprefix = "";
    })
  else
    let p_auth = authorized_age conf base p in
    let index = Int32.of_int (Adef.int_of_iper (get_key_index p)) in
    let sex =
      match get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      let sosa_nb = Perso.get_sosa_person conf base p in
      if Sosa.eq sosa_nb Sosa.zero then `no_sosa
      else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
      else `sosa
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_surname p))
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_first_name p))
    in
    let occ = Int32.of_int (get_occ p) in
    let (first_name, surname) =
      if not p_auth && (is_hide_names conf p) then ("x", "x")
      else person_firstname_surname_txt conf base p
    in
    let (birth_short, birth_place, death_short, death_place) =
      if p_auth then
        let (birth, death, _) = Date.get_birth_death_date p in
        let birth =
          match birth with
          | Some d -> Date.string_slash_of_date conf d
          | None -> ""
        in
        let birth_place =
          let birth_place = sou base (get_birth_place p) in
          if birth_place <> "" then Util.string_of_place conf birth_place
          else
            let baptism_place = sou base (get_baptism_place p) in
            Util.string_of_place conf baptism_place
        in
        let death =
          match death with
          | Some d -> Date.string_slash_of_date conf d
          | None -> ""
        in
        let death_place =
          let death_place = sou base (get_death_place p) in
          if death_place <> "" then Util.string_of_place conf death_place
          else
            let burial_place = sou base (get_burial_place p) in
            Util.string_of_place conf burial_place
        in
        (birth, birth_place, death, death_place)
      else ("", "", "", "")
    in
    let image =
      if not conf.no_image && p_auth then
        let img = sou base (get_image p) in
        if img <> "" then img
        else
          match Api_util.find_image_file conf base p with
          | Some s -> "1"
          | None -> ""
      else ""
    in
    Mread.Simple_person.({
      index = index;
      sex = sex;
      lastname = surname;
      firstname = first_name;
      n = sn;
      p = fn;
      occ = occ;
      birth_short_date = if birth_short = "" then None else Some birth_short;
      birth_place = if birth_place = "" then None else Some birth_place;
      death_short_date = if death_short = "" then None else Some death_short;
      death_place = if death_place = "" then None else Some death_place;
      image = if image = "" then None else Some image;
      sosa = sosa;
      baseprefix = base_prefix
    })
;;


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family_link : config -> base -> ifam -> Family    *)
(** [Description] : Retourne à partir d'une famille distante une Family
                    (piqi app) dont tous les champs sont complétés.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      - Family : Retourne une famille dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family_link conf base ip ifath imoth sp ifam fam fam_link =
  let base_prefix = fam_link.MLink.Family.baseprefix in
  let spouse = pers_to_piqi_simple_person conf base sp base_prefix in
  let p_auth = true in
  let m_auth = true in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = Int32.of_int (Adef.int_of_ifam gen_f.fam_index) in
  let (marriage_date, marriage_date_conv, marriage_cal) =
    match (m_auth, Adef.od_of_codate gen_f.marriage) with
    | (true, Some d) -> string_of_date_and_conv conf d
    | _ -> ("", "", None)
  in
  let marriage_place =
    if m_auth then Util.string_of_place conf gen_f.marriage_place else ""
  in
  let marriage_src = if p_auth then gen_f.marriage_src else "" in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
  in
  let (divorce_type, divorce_date, divorce_date_conv, divorce_cal) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, "", "", None)
    | Divorced cod ->
        (match Adef.od_of_codate cod with
         | Some d when m_auth ->
             let (divorce_date, divorce_date_conv, divorce_cal) =
               string_of_date_and_conv conf d
             in
             (`divorced, divorce_date, divorce_date_conv, divorce_cal)
         | _ -> (`divorced, "", "", None))
    | Separated -> (`separated, "", "", None)
  in
  let witnesses =
    List.map
      (fun ip ->
        let p = poi base ip in
        pers_to_piqi_simple_person conf base p base_prefix)
      (Array.to_list gen_f.witnesses)
  in
  let notes =
    if m_auth && not conf.no_note then
      let s = gen_f.comment in
      let s = string_with_macros conf [] s in
      let lines = Api_wiki.html_of_tlsw conf s in
      let wi =
        {Api_wiki.wi_mode = "NOTES";
         Api_wiki.wi_cancel_links = conf.cancel_links;
         Api_wiki.wi_file_path = Notes.file_path conf base;
         Api_wiki.wi_person_exists = person_exists conf base;
         Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      let s = Api_wiki.syntax_links conf wi (String.concat "\n" lines) in
      if conf.pure_xhtml then Util.check_xhtml s else s
    else ""
  in
  let fsources =
    if m_auth then
      let s = gen_f.fsources in
      let s =
        let wi =
          {Api_wiki.wi_mode = "NOTES";
           Api_wiki.wi_cancel_links = conf.cancel_links;
           Api_wiki.wi_file_path = Notes.file_path conf base;
           Api_wiki.wi_person_exists = person_exists conf base;
           Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
        in
        Api_wiki.syntax_links conf wi s
      in
      let s = string_with_macros conf [] s in
      s
    else ""
  in
  let children =
    List.map
      (fun c_link ->
        let base_prefix = c_link.MLink.Person.baseprefix in
        let (p, _) = Perso_link.make_ep_link conf base c_link in
        pers_to_piqi_simple_person conf base p base_prefix)
      (Perso_link.get_children_of_parents base_prefix ifam ifath imoth)
  in
  Mread.Family.({
    index = index;
    spouse = spouse;
    marriage_date = if marriage_date = "" then None else Some marriage_date;
    marriage_date_conv =
      if marriage_date_conv = "" then None else Some marriage_date_conv;
    marriage_date_cal = marriage_cal;
    marriage_place = if marriage_place = "" then None else Some marriage_place;
    marriage_src = if marriage_src = "" then None else Some marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = if divorce_date = "" then None else Some divorce_date;
    divorce_date_conv =
      if divorce_date_conv = "" then None else Some divorce_date_conv;
    divorce_date_cal = divorce_cal;
    witnesses = witnesses;
    notes = if notes = "" then None else Some notes;
    fsources = if fsources = "" then None else Some fsources;
    children = children;
  })
;;


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family : config -> base -> ifam -> Family         *)
(** [Description] : Retourne à partir d'une ifam (gwdb) une Family
                    (piqi app) dont tous les champs sont complétés.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      - Family : Retourne une famille dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family conf base p ifam =
  let base_prefix = conf.command in
  let fam = foi base ifam in
  let sp = poi base (Gutil.spouse (get_key_index p) fam) in
  let spouse = pers_to_piqi_simple_person conf base sp base_prefix in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let p_auth = authorized_age conf base p in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = Int32.of_int (Adef.int_of_ifam gen_f.fam_index) in
  let (marriage_date, marriage_date_conv, marriage_cal) =
    match (m_auth, Adef.od_of_codate gen_f.marriage) with
    | (true, Some d) -> string_of_date_and_conv conf d
    | _ -> ("", "", None)
  in
  let marriage_place =
    if m_auth then Util.string_of_place conf gen_f.marriage_place else ""
  in
  let marriage_src = if p_auth then gen_f.marriage_src else "" in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
  in
  let (divorce_type, divorce_date, divorce_date_conv, divorce_cal) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, "", "", None)
    | Divorced cod ->
        (match Adef.od_of_codate cod with
         | Some d when m_auth ->
             let (divorce_date, divorce_date_conv, divorce_cal) =
               string_of_date_and_conv conf d
             in
             (`divorced, divorce_date, divorce_date_conv, divorce_cal)
         | _ -> (`divorced, "", "", None))
    | Separated -> (`separated, "", "", None)
  in
  let witnesses =
    List.map
      (fun ip ->
        let p = poi base ip in
        pers_to_piqi_simple_person conf base p base_prefix)
      (Array.to_list gen_f.witnesses)
  in
  let notes =
    if m_auth && not conf.no_note then
      let s = gen_f.comment in
      let s = string_with_macros conf [] s in
      let lines = Api_wiki.html_of_tlsw conf s in
      let wi =
        {Api_wiki.wi_mode = "NOTES";
         Api_wiki.wi_cancel_links = conf.cancel_links;
         Api_wiki.wi_file_path = Notes.file_path conf base;
         Api_wiki.wi_person_exists = person_exists conf base;
         Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
      in
      let s = Api_wiki.syntax_links conf wi (String.concat "\n" lines) in
      if conf.pure_xhtml then Util.check_xhtml s else s
    else ""
  in
  let fsources =
    if m_auth then
      let s = gen_f.fsources in
      let s =
        let wi =
          {Api_wiki.wi_mode = "NOTES";
           Api_wiki.wi_cancel_links = conf.cancel_links;
           Api_wiki.wi_file_path = Notes.file_path conf base;
           Api_wiki.wi_person_exists = person_exists conf base;
           Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
        in
        Api_wiki.syntax_links conf wi s
      in
      let s = string_with_macros conf [] s in
      s
    else ""
  in
  let children =
    List.map
      (fun ip ->
        let p = poi base ip in
        pers_to_piqi_simple_person conf base p base_prefix)
      (Array.to_list (get_children fam))
  in
  (* lien inter arbre *)
  let children_link =
    let family_link =
      Perso_link.get_families_of_parents
        conf.command (get_key_index p) (get_key_index sp)
    in
    List.fold_right
      (fun fam_link accu ->
         List.fold_right
           (fun c_link accu ->
              let baseprefix = c_link.MLink.Person_link.baseprefix in
              let ip_c =
                Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
              in
              match Perso_link.get_person_link baseprefix ip_c with
              | Some c_link ->
                  let can_merge =
                    Perso_link.can_merge_child conf.command
                       (get_children fam) c_link
                  in
                  if can_merge then accu
                  else
                    let (p, _) = Perso_link.make_ep_link conf base c_link in
                    pers_to_piqi_simple_person conf base p baseprefix :: accu
              | None -> accu)
           fam_link.MLink.Family.children accu)
      family_link []
  in
  let children = children @ children_link in
  Mread.Family.({
    index = index;
    spouse = spouse;
    marriage_date = if marriage_date = "" then None else Some marriage_date;
    marriage_date_conv =
      if marriage_date_conv = "" then None else Some marriage_date_conv;
    marriage_date_cal = marriage_cal;
    marriage_place = if marriage_place = "" then None else Some marriage_place;
    marriage_src = if marriage_src = "" then None else Some marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = if divorce_date = "" then None else Some divorce_date;
    divorce_date_conv =
      if divorce_date_conv = "" then None else Some divorce_date_conv;
    divorce_date_cal = divorce_cal;
    witnesses = witnesses;
    notes = if notes = "" then None else Some notes;
    fsources = if fsources = "" then None else Some fsources;
    children = children;
  })
;;


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person : config -> base -> person -> Person           *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person (piqi)
                    dont tous les champs sont complétés.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person conf base p =
  if is_restricted conf base (get_key_index p) then
    Mread.Person.({
      index = Int32.of_int (-1);
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      public_name = None;
      aliases = [];
      qualifiers = [];
      firstname_aliases = [];
      surname_aliases = [];
      image = None;
      birth_date = None;
      birth_date_conv = None;
      birth_date_cal = None;
      birth_place = None;
      birth_src = None;
      baptism_date = None;
      baptism_date_conv = None;
      baptism_date_cal = None;
      baptism_place = None;
      baptism_src = None;
      death_date = None;
      death_date_conv = None;
      death_date_cal = None;
      death_place = None;
      death_src = None;
      death_type = `dont_know_if_dead;
      burial_date = None;
      burial_date_conv = None;
      burial_date_cal = None;
      burial_place = None;
      burial_src = None;
      occupation = None;
      notes = None;
      psources = None;
      has_sources = false;
      titles = [];
      related = [];
      rparents = [];
      father = None;
      mother = None;
      families = [];
      sosa = `no_sosa;
      events = [];
      events_witnesses = [];
    })
  else
    let base_prefix = conf.command in
    let p_auth = authorized_age conf base p in
    let gen_p = Util.string_gen_person base (gen_person_of_person p) in
    let index = Int32.of_int (Adef.int_of_iper (get_key_index p)) in
    let sex =
      match get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      let sosa_nb = Perso.get_sosa_person conf base p in
      if Sosa.eq sosa_nb Sosa.zero then `no_sosa
      else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
      else `sosa
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_surname p))
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower (sou base (get_first_name p))
    in
    let occ = Int32.of_int (get_occ p) in
    let surname =
      if not p_auth && (is_hide_names conf p) then "x" else gen_p.surname
    in
    let first_name =
      if not p_auth && (is_hide_names conf p) then "x" else gen_p.first_name
    in
    let publicname = if not p_auth then "" else gen_p.public_name in
    let aliases = if not p_auth then [] else gen_p.aliases in
    let qualifiers = if not p_auth then [] else gen_p.qualifiers in
    let firstname_aliases =
      if not p_auth then [] else gen_p.first_names_aliases
    in
    let surname_aliases = if not p_auth then [] else gen_p.surnames_aliases in
    let image =
      if not conf.no_image && p_auth then
        let img = sou base (get_image p) in
        if img <> "" then img
        else
          match Api_util.find_image_file conf base p with
          | Some s -> "1"
          | None -> ""
      else ""
    in
    let (birth, birth_conv, birth_cal) =
      match (p_auth, Adef.od_of_codate gen_p.birth) with
      | (true, Some d) -> string_of_date_and_conv conf d
      | _ -> ("", "", None)
    in
    let birth_place =
      if p_auth then Util.string_of_place conf gen_p.birth_place else ""
    in
    let birth_src = if p_auth then gen_p.birth_src else "" in
    let (baptism, baptism_conv, baptism_cal) =
      match (p_auth, Adef.od_of_codate gen_p.baptism) with
      | (true, Some d) -> string_of_date_and_conv conf d
      | _ -> ("", "", None)
    in
    let baptism_place =
      if p_auth then Util.string_of_place conf gen_p.baptism_place else ""
    in
    let baptism_src = if p_auth then gen_p.baptism_src else "" in
    let (death_type, death, death_conv, death_cal) =
      match (p_auth, gen_p.death) with
      | (true, NotDead) -> (`not_dead, "", "", None)
      | (true, Death (_, cd)) ->
          let d = Adef.date_of_cdate cd in
          let (death, death_conv, death_cal) = string_of_date_and_conv conf d in
          (`dead, death, death_conv, death_cal)
      | (true, DeadYoung) -> (`dead_young, "", "", None)
      | (true, DeadDontKnowWhen) -> (`dead_dont_know_when, "", "", None)
      | (true, DontKnowIfDead) -> (`dont_know_if_dead, "", "", None)
      | (true, OfCourseDead) -> (`of_course_dead, "", "", None)
      | _ -> (`dont_know_if_dead, "", "", None)
    in
    let death_place =
      if p_auth then Util.string_of_place conf gen_p.death_place else ""
    in
    let death_src = if p_auth then gen_p.death_src else "" in
    let (burial, burial_conv, burial_cal) =
      match (p_auth, gen_p.burial) with
      | (true, Buried cod) | (true, Cremated cod) ->
          (match Adef.od_of_codate cod with
          | Some d -> string_of_date_and_conv conf d
          | _ -> ("", "", None))
      | _ -> ("", "", None)
    in
    let burial_place =
      if p_auth then Util.string_of_place conf gen_p.burial_place else ""
    in
    let burial_src = if p_auth then gen_p.burial_src else "" in
    let occupation =
      if p_auth then
        let s = gen_p.occupation in
        let s =
          let wi =
            {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
             Wiki.wi_file_path = Notes.file_path conf base;
             Wiki.wi_person_exists = person_exists conf base;
             Wiki.wi_always_show_link = conf.wizard || conf.friend}
          in
          Wiki.syntax_links conf wi s
        in
        string_with_macros conf [] s
      else ""
    in
    let events =
      if p_auth then
        List.map
          (fun (name, date, place, note, src, w, isp) ->
            let (name, is_occu) =
              match name with
              | Perso.Pevent Epers_Occupation ->
                  if conf.no_note || String.length (sou base note) > 100 then
                    (Util.string_of_pevent_name conf base Epers_Occupation, false)
                  else
                    let s = sou base note in
                    (* Si il y a des <br>, on les supprime. *)
                    let s = Str.global_replace (Str.regexp "<br */?>") "" s in
                    (s, true)
              | Perso.Pevent name -> (Util.string_of_pevent_name conf base name, false)
              | Perso.Fevent name -> (Util.string_of_fevent_name conf base name, false)
            in
            let (date, date_conv, date_cal) =
              match Adef.od_of_codate date with
              | Some d -> string_of_date_and_conv conf d
              | _ -> ("", "", None)
            in
            let place = Util.string_of_place conf (sou base place) in
            let note =
              if is_occu then ""
              else if not conf.no_note then
                begin
                  let env = [('i', fun () -> Util.default_image_name base p)] in
                  let s = sou base note in
                  let s = string_with_macros conf env s in
                  let lines = Api_wiki.html_of_tlsw conf s in
                  let wi =
                    {Api_wiki.wi_mode = "NOTES"; Api_wiki.wi_cancel_links = conf.cancel_links;
                     Api_wiki.wi_file_path = Notes.file_path conf base;
                     Api_wiki.wi_person_exists = person_exists conf base;
                     Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
                  in
                  let s = Api_wiki.syntax_links conf wi (String.concat "\n" lines) in
                  if conf.pure_xhtml then Util.check_xhtml s else s
                end
              else ""
            in
            let src =
              let s = sou base src in
              let env = [('i', fun () -> Util.default_image_name base p)] in
              let s =
                let wi =
                  {Api_wiki.wi_mode = "NOTES";
                   Api_wiki.wi_cancel_links = conf.cancel_links;
                   Api_wiki.wi_file_path = Notes.file_path conf base;
                   Api_wiki.wi_person_exists = person_exists conf base;
                   Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
                in
                Api_wiki.syntax_links conf wi s
              in
              string_with_macros conf env s
            in
            let spouse =
              match isp with
              | Some ip ->
                  let sp = poi base ip in
                  Some (pers_to_piqi_simple_person conf base sp base_prefix)
              | None -> None
            in
            let witnesses =
              List.map
                (fun (ip, wk) ->
                   let witness_type =
                     match wk with
                     | Witness -> `witness
                     | Witness_GodParent -> `witness_godparent
                   in
                   let witness = poi base ip in
                   let witness =
                     pers_to_piqi_simple_person conf base witness base_prefix
                   in
                   Mread.Witness_event.({
                     witness_type = witness_type;
                     witness = witness;
                   }))
                (Array.to_list w)
            in
            Mread.Event.({
              name = name;
              date = if date = "" then None else Some date;
              date_conv = if date_conv = "" then None else Some date_conv;
              date_cal = date_cal;
              place = if place = "" then None else Some place;
              reason = None;
              note = if note = "" then None else Some note;
              src = if src= "" then None else Some src;
              spouse = spouse;
              witnesses = witnesses;
            }))
          (Perso.events_list conf base p)
      else []
    in
    let has_relations =
      if p_auth && conf.use_restrict then
        let related =
          List.fold_left
            (fun l ip ->
               let rp = pget conf base ip in
               if is_hidden rp then l else (ip :: l))
          [] (get_related p)
        in
        get_rparents p <> [] || related <> []
      else p_auth && (get_rparents p <> [] || get_related p <> [])
    in
    let events_witnesses =
      if has_relations then
        begin
          let array_mem_witn x a =
            let rec loop i =
              if i = Array.length a then (false, "")
              else if x = fst a.(i) then
                let witness_kind =
                  Util.string_of_witness_kind conf (poi base x) (snd a.(i))
                in
                (true, witness_kind)
              else loop (i + 1)
            in
            loop 0
          in
          let related = Mutil.list_uniq (List.sort compare gen_p.related) in
          let events_witnesses =
            let list = ref [] in
            let rec make_list =
              function
              | ic :: icl ->
                  let c = pget conf base ic in
                  List.iter
                    (fun ((name, _, _, _, _, wl, _) as evt) ->
                      let (mem, wk) = array_mem_witn (get_key_index p) wl in
                      if mem then
                        (* Attention aux doublons pour les evenements famille. *)
                        match name with
                        | Perso.Fevent _ ->
                            if get_sex c = Male then
                              list := (c, wk, evt) :: !list
                            else ()
                        | _ -> list := (c, wk, evt) :: !list
                      else ())
                    (Perso.events_list conf base c);
                  make_list icl
              | [] -> ()
            in
            make_list related;
            !list
          in
          (* On tri les témoins dans le même ordre que les évènements. *)
          let events_witnesses =
            CheckItem.sort_events
              ((fun (_, _, (name, _, _, _, _, _, _)) ->
                match name with
                | Perso.Pevent n -> CheckItem.Psort n
                | Perso.Fevent n -> CheckItem.Fsort n),
               (fun (_, _, (_, date, _, _, _, _, _)) -> date))
              events_witnesses
          in
          List.map
            (fun (p, wk, (name, date, place, note, src, wl, isp)) ->
              let witness_date =
                match Adef.od_of_codate date with
                | Some (Dgreg (dmy, _)) -> " (" ^ Date.year_text dmy ^ ")"
                | _ -> ""
              in
              let witnesses_name =
                match name with
                | Perso.Pevent name ->
                    if p_auth then Util.string_of_pevent_name conf base name
                    else  ""
                | Perso.Fevent name ->
                    if p_auth then Util.string_of_fevent_name conf base name
                    else  ""
              in
              let event_witness_type =
                capitale wk ^ witness_date ^ ": " ^ witnesses_name
              in
              let husband = pers_to_piqi_simple_person conf base p base_prefix in
              let wife =
                match isp with
                | Some isp ->
                    let sp = poi base isp in
                    Some (pers_to_piqi_simple_person conf base sp base_prefix)
                | None -> None
              in
              Mread.Event_witness.({
                event_witness_type = event_witness_type;
                husband = husband;
                wife = wife;
              }) )
            events_witnesses
        end
      else []
    in
    let notes =
      if p_auth && not conf.no_note then
        let env = [('i', fun () -> Util.default_image_name base p)] in
        let s = gen_p.notes in
        let s = string_with_macros conf env s in
        let lines = Api_wiki.html_of_tlsw conf s in
        let wi =
          {Api_wiki.wi_mode = "NOTES"; Api_wiki.wi_cancel_links = conf.cancel_links;
           Api_wiki.wi_file_path = Notes.file_path conf base;
           Api_wiki.wi_person_exists = person_exists conf base;
           Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
        in
        let s = Api_wiki.syntax_links conf wi (String.concat "\n" lines) in
        if conf.pure_xhtml then Util.check_xhtml s else s
      else ""
    in
    let psources =
      if not (is_hide_names conf p) || p_auth then
        let s = gen_p.psources in
        let env = [('i', fun () -> Util.default_image_name base p)] in
        let s =
          let wi =
            {Api_wiki.wi_mode = "NOTES";
             Api_wiki.wi_cancel_links = conf.cancel_links;
             Api_wiki.wi_file_path = Notes.file_path conf base;
             Api_wiki.wi_person_exists = person_exists conf base;
             Api_wiki.wi_always_show_link = conf.wizard || conf.friend}
          in
          Api_wiki.syntax_links conf wi s
        in
        let s = string_with_macros conf env s in
        s
      else ""
    in
    let has_sources =
      if (is_hide_names conf p) && not p_auth then false
      else if psources <> "" then true
      else if
        p_auth &&
        (birth_src <> "" || baptism_src <> "" ||
         death_src <> "" || burial_src <> "")
      then true
      else false
    in
    let titles = Perso.nobility_titles_list conf base p in
    let titles =
      let tmp_conf = {(conf) with cancel_links = true} in
      List.map (Perso.string_of_title tmp_conf base "" p) titles
    in
    let related =
      if has_relations then
        let list =
          let list = Mutil.list_uniq (List.sort compare (gen_p.related)) in
          List.fold_left
            (fun list ic ->
               let c = pget conf base ic in
               let rec loop list =
                 function
                 | r :: rl ->
                     (match r.r_fath with
                     | Some ip when ip = get_key_index p ->
                         loop ((c, r) :: list) rl
                     | _ ->
                         (match r.r_moth with
                         | Some ip when ip = get_key_index p ->
                             loop ((c, r) :: list) rl
                         | _ -> loop list rl))
                 | [] -> list
               in loop list (get_rparents c))
            [] list
        in
        let list =
          List.sort
            (fun (c1, _) (c2, _) ->
               let d1 =
                 match Adef.od_of_codate (get_baptism c1) with
                 | None -> Adef.od_of_codate (get_birth c1)
                 | x -> x
               in
               let d2 =
                 match Adef.od_of_codate (get_baptism c2) with
                 | None -> Adef.od_of_codate (get_birth c2)
                 | x -> x
               in
               match (d1, d2) with
               |(Some d1, Some d2) ->
                   if CheckItem.strictly_before d1 d2 then -1 else 1
               | _ -> -1 )
          (List.rev list)
        in
        List.map
          (fun (p, rp) ->
            let p = pers_to_piqi_simple_person conf base p base_prefix in
            let r_type =
              match rp.r_type with
              | Adoption -> `rchild_adoption
              | Recognition -> `rchild_recognition
              | CandidateParent -> `rchild_candidate_parent
              | GodParent -> `rchild_god_parent
              | FosterParent -> `rchild_foster_parent
            in
            Mread.Relation_person.({
              r_type = r_type;
              person = p;
            }) )
          list
      else []
    in
    let rparents =
      if has_relations then
        List.fold_left
          (fun rl rp ->
            let r_type =
              match rp.r_type with
              | Adoption -> `rparent_adoption
              | Recognition -> `rparent_recognition
              | CandidateParent -> `rparent_candidate_parent
              | GodParent -> `rparent_god_parent
              | FosterParent -> `rparent_foster_parent
            in
            let rl =
              match rp.r_fath with
              | Some ip ->
                  let p = poi base ip in
                  let p = pers_to_piqi_simple_person conf base p base_prefix in
                  let p =
                    Mread.Relation_person.({
                      r_type = r_type;
                      person = p;
                    })
                  in
                  p :: rl
              | None -> rl
            in
            let rl =
              match rp.r_moth with
              | Some ip ->
                  let p = poi base ip in
                  let p = pers_to_piqi_simple_person conf base p base_prefix in
                  let p =
                    Mread.Relation_person.({
                      r_type = r_type;
                      person = p;
                    })
                  in
                  p :: rl
              | None -> rl
            in
            rl)
          [] gen_p.rparents
      else []
    in
    (*
    let was_witness =
      let list =
        let list = ref [] in
        let related = Mutil.list_uniq (List.sort compare (gen_p.related)) in
        let rec make_list =
          function
          | ic :: icl ->
              let c = pget conf base ic in
              if get_sex c = Male then
                Array.iter
                  (fun ifam ->
                     let fam = foi base ifam in
                     if Mutil.array_mem (get_key_index p) (get_witnesses fam)
                     then
                       list := (ifam, fam) :: !list
                     else ())
                  (get_family (pget conf base ic))
              else ();
              make_list icl
          | [] -> ()
        in
        make_list related;
        !list
      in
      let list =
        List.sort
          (fun (_, fam1) (_, fam2) ->
             match
               (Adef.od_of_codate (get_marriage fam1),
                Adef.od_of_codate (get_marriage fam2))
             with
             | (Some d1, Some d2) ->
                 if CheckItem.strictly_before d1 d2 then -1
                 else if CheckItem.strictly_before d2 d1 then 1
                 else 0
             | _ -> 0 )
          list
      in
      List.map
        (fun (ifam, fam) ->
           let ifath = get_father fam in
           let imoth = get_mother fam in
           let father = poi base ifath in
           let mother = poi base imoth in
           let father_auth = authorized_age conf base father in
           let husband =
             if not father_auth && (is_hide_names conf father) then "x x"
             else p_first_name base father ^ " " ^ p_surname base father
           in
           let mother_auth = authorized_age conf base mother in
           let wife =
             if not mother_auth && (is_hide_names conf mother) then "x x"
             else p_first_name base mother ^ " " ^ p_surname base mother
           in
           (*
           let husband = pers_to_piqi_simple_person conf base father in
           let wife = pers_to_piqi_simple_person conf base mother in
           *)
           Mread.Was_witness.({
             husband = husband;
             wife = wife;
           }) )
        list
    in
    *)
    let (father, mother) =
      match get_parents p with
      | Some ifam ->
          let cpl = foi base ifam in
          let ifath = get_father cpl in
          let imoth = get_mother cpl in
          let father =
            if (Adef.int_of_iper ifath) < 0 then None
            else
              let father = poi base ifath in
              Some (pers_to_piqi_simple_person conf base father base_prefix)
          in
          let mother =
            if (Adef.int_of_iper imoth) < 0 then None
            else
              let mother = poi base imoth in
              Some (pers_to_piqi_simple_person conf base mother base_prefix)
          in
          (father, mother)
      | None ->
          (* lien inter arbre *)
          let ip = get_key_index p in
          let base_prefix = conf.command in
          match Perso_link.get_parents_link base_prefix ip with
          | Some family ->
              begin
                let ifath = Adef.iper_of_int (Int32.to_int family.MLink.Family.ifath) in
                let imoth = Adef.iper_of_int (Int32.to_int family.MLink.Family.imoth) in
                let fam_base_prefix = family.MLink.Family.baseprefix in
                match
                  (Perso_link.get_person_link fam_base_prefix ifath,
                   Perso_link.get_person_link fam_base_prefix imoth,
                   Perso_link.get_person_link base_prefix ip)
                with
                | (Some pfath, Some pmoth, Some c) ->
                    let (fath, _) = Perso_link.make_ep_link conf base pfath in
                    let (moth, _) = Perso_link.make_ep_link conf base pmoth in
                    let father =
                      Some (pers_to_piqi_simple_person conf base fath fam_base_prefix)
                    in
                    let mother =
                      Some (pers_to_piqi_simple_person conf base moth fam_base_prefix)
                    in
                    (father, mother)
                | _ -> (None, None)
              end
          | None -> (None, None)
    in
    let families =
      List.map
        (fun ifam -> fam_to_piqi_family conf base p ifam)
        (Array.to_list (get_family p))
    in
    (* lien inter arbre *)
    let families_link =
      let ip = get_key_index p in
      let base_prefix = conf.command in
      let families = Perso_link.get_family_link base_prefix ip in
      List.fold_right
        (fun fam_link accu ->
           let (ifam, fam, _, _) =
             Perso_link.make_efam_link conf base ip fam_link
           in
           let (ifath, imoth, ifam) =
             (Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.ifath),
              Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.imoth),
              Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam))
           in
           let cpl =
             let ip = get_key_index p in
             if ip <> ifath && ip <> imoth then
               match
                 Perso_link.get_person_link_with_base
                   conf.command ip fam_link.MLink.Family.baseprefix
               with
               | Some p ->
                   let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
                   (ifath, imoth, if ip = ifath then imoth else ifath)
               | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
             else (ifath, imoth, if ip = ifath then imoth else ifath)
           in
           let can_merge =
             let fam = List.map (foi base) (Array.to_list (get_family p)) in
             Perso_link.can_merge_family conf.command (get_key_index p) fam fam_link cpl
           in
           if can_merge then accu
           else
             let (ifath, imoth, isp) = cpl in
             match
               (Perso_link.get_person_link fam_link.MLink.Family.baseprefix ifath,
                Perso_link.get_person_link fam_link.MLink.Family.baseprefix imoth,
                Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp)
             with
             | (Some fath, Some moth, Some sp) ->
                 let (sp, _) = Perso_link.make_ep_link conf base sp in
                 fam_to_piqi_family_link conf base ip ifath imoth sp ifam fam fam_link :: accu
             | _ -> accu)
        families []
    in
    let families = families @ families_link in
    Mread.Person.({
      index = index;
      sex = sex;
      lastname = surname;
      firstname = first_name;
      n = sn;
      p = fn;
      occ = occ;
      public_name = if publicname = "" then None else Some publicname;
      aliases = aliases;
      qualifiers = qualifiers;
      firstname_aliases = firstname_aliases;
      surname_aliases = surname_aliases;
      image = if image = "" then None else Some image;
      birth_date = if birth = "" then None else Some birth;
      birth_date_conv = if birth_conv = "" then None else Some birth_conv;
      birth_date_cal = birth_cal;
      birth_place = if birth_place = "" then None else Some birth_place;
      birth_src = if birth_src = "" then None else Some birth_src;
      baptism_date = if baptism = "" then None else Some baptism;
      baptism_date_conv = if baptism_conv = "" then None else Some baptism_conv;
      baptism_date_cal = baptism_cal;
      baptism_place = if baptism_place = "" then None else Some baptism_place;
      baptism_src = if baptism_src = "" then None else Some baptism_src;
      death_date = if death = "" then None else Some death;
      death_date_conv = if death_conv = "" then None else Some death_conv;
      death_date_cal = death_cal;
      death_place = if death_place = "" then None else Some death_place;
      death_src = if death_src = "" then None else Some death_src;
      death_type = death_type;
      burial_date = if burial = "" then None else Some burial;
      burial_date_conv = if burial_conv = "" then None else Some burial_conv;
      burial_date_cal = burial_cal;
      burial_place = if burial_place = "" then None else Some burial_place;
      burial_src = if burial_src = "" then None else Some burial_src;
      occupation = if occupation = "" then None else Some occupation;
      notes = if notes = "" then None else Some notes;
      psources = if psources = "" then None else Some psources;
      has_sources = has_sources;
      titles = titles;
      related = related;
      rparents = rparents;
      father = father;
      mother = mother;
      families = families;
      sosa = sosa;
      events = events;
      events_witnesses = events_witnesses;
    })
;;


(* ********************************************************************* *)
(*  [Fonc] print_person_tree : conf -> base -> unit                      *)
(** [Description] : Renvoie un objet personne qui servira a afficher
      toutes les informations sur le panneau latéral de l'arbre de
      navigation.
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_person_tree conf base =
  let ip = get_params conf Mext_read.parse_index_person in
  let ip = Adef.iper_of_int (Int32.to_int ip.Mread.Index_person.index) in
  let () = Perso.build_sosa_ht conf base in
  let p = poi base ip in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let pers_piqi = pers_to_piqi_person conf base p in
  let data = Mext_read.gen_person pers_piqi in
  print_result conf data
;;


(**/**) (* V1 *)

(* Graphe d'ascendance *)

let build_graph_asc conf base p max_gen base_loop =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge p_from p_to =
    Mread.Edge.({
      from_node = Int64.of_int (Adef.int_of_iper (get_key_index p_from));
      to_node = Int64.of_int (Adef.int_of_iper (get_key_index p_to));
    })
  in
  let create_node p gen more_info base_prefix =
    let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
    let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
    Mread.Node.({
      id = id;
      person = p;
      ifam = None;
    })
  in
(*
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
*)
  let rec loop l nodes edges families =
    match l with
    | [] ->
        (* On retourne la liste pour avoir les noeuds dans l'ordre *)
        (* la référence, suivi du père suivi, puis de la mère ...  *)
        (List.rev !nodes, List.rev !edges, List.rev !families)
    | (p, gen) :: l ->
        try
          let _ = Hashtbl.find ht (get_key_index p) in
          loop l nodes edges families
        with Not_found ->
          begin
            if gen >= max_gen then
              loop l nodes edges families
            else
              begin
                Hashtbl.add ht (get_key_index p) true;
                match get_parents p with
                | Some ifam ->
                    let cpl = foi base ifam in
                    let fath = poi base (get_father cpl) in
                    let moth = poi base (get_mother cpl) in
                    nodes := create_node fath gen Ancestor conf.command :: !nodes;
                    nodes := create_node moth gen Ancestor conf.command :: !nodes;
                    edges := (create_edge p fath) :: !edges;
                    edges := (create_edge p moth) :: !edges;
                    (*create_family ifam families;*)
                    loop ((fath, gen + 1) :: (moth, gen + 1) :: l) nodes edges families
                | None ->
                    (* lien inter arbre *)
                    let ip = get_key_index p in
                    let () =
                      Perso_link.init_cache conf base ip (max_gen - gen) 0 0
                    in
                    let (nodes, edges) =
                      let rec loop_parents nodes edges l =
                        match l with
                        | [] -> (nodes, edges)
                        | (base_prefix, p, gen) :: l ->
                            if gen >= max_gen then loop_parents nodes edges l
                            else
                              let ip = get_key_index p in
                              match Perso_link.get_parents_link base_prefix ip with
                              | Some family ->
                                  begin
                                  let ifath = Adef.iper_of_int (Int32.to_int family.MLink.Family.ifath) in
                                  let imoth = Adef.iper_of_int (Int32.to_int family.MLink.Family.imoth) in
                                  let fam_base_prefix = family.MLink.Family.baseprefix in
                                  match
                                    (Perso_link.get_person_link fam_base_prefix ifath,
                                     Perso_link.get_person_link fam_base_prefix imoth,
                                     Perso_link.get_person_link base_prefix ip)
                                  with
                                  | (Some pfath, Some pmoth, Some c) ->
                                      let (fath, _) = Perso_link.make_ep_link conf base pfath in
                                      let (moth, _) = Perso_link.make_ep_link conf base pmoth in
                                      nodes := create_node fath gen Ancestor pfath.MLink.Person.baseprefix :: !nodes;
                                      nodes := create_node moth gen Ancestor pmoth.MLink.Person.baseprefix :: !nodes;
                                      edges := (create_edge p fath) :: !edges;
                                      edges := (create_edge p moth) :: !edges;
                                      let l =
                                        ((fam_base_prefix, fath, gen + 1) :: (fam_base_prefix, moth, gen + 1) :: l)
                                      in
                                      loop_parents nodes edges l
                                  | _ -> loop_parents nodes edges l
                                  end
                              | None -> loop_parents nodes edges l
                      in
                      loop_parents nodes edges [(conf.bname, p, gen)]
                    in
                    loop l nodes edges families
              end
          end
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  nodes := create_node p 1 Root conf.command :: !nodes;
  loop [(p, 1)] nodes edges families
;;


(* Graphe de descendance *)

let build_graph_desc conf base p max_gen base_loop =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge p_from p_to =
    Mread.Edge.({
      from_node = Int64.of_int (Adef.int_of_iper (get_key_index p_from));
      to_node = Int64.of_int (Adef.int_of_iper (get_key_index p_to));
    })
  in
  let create_node p ifam gen more_info base_prefix =
    let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
    let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
    let ifam = Int64.of_int (Adef.int_of_ifam ifam) in
    Mread.Node.({
      id = id;
      person = p;
      ifam = Some ifam;
    })
  in
(*
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
*)
  let rec loop l nodes edges families =
    match l with
    | [] ->
        (* On retourne la liste pour avoir les noeuds dans l'ordre *)
        (* la référence, suivi du père suivi, puis de la mère ...  *)
        (List.rev !nodes, List.rev !edges, !families)
    | (p, gen) :: l ->
        try
          let _ = Hashtbl.find ht (get_key_index p) in
          loop l nodes edges families
        with Not_found ->
          begin
            if gen >= max_gen then
              loop l nodes edges families
            else
              begin
                Hashtbl.add ht (get_key_index p) true;
                let ifam = get_family p in
                let l =
                  List.fold_left
                    (fun accu ifam  ->
                      let fam = foi base ifam in
                      let sp = poi base (Gutil.spouse (get_key_index p) fam) in
                      let children =
                        List.map (poi base) (Array.to_list (get_children fam))
                      in
                      nodes := (create_node sp ifam gen Spouse conf.command) :: !nodes;
                      edges := (create_edge p sp) :: !edges;
                      if gen <> max_gen then
                        begin
                          nodes :=
                            List.fold_left
                              (fun nodes c -> create_node c ifam gen Children conf.command :: nodes)
                              !nodes children;
                          List.iter
                            (fun c ->
                              edges := (create_edge p c) :: !edges;
                              edges := (create_edge sp c) :: !edges)
                            children;
                          (*create_family ifam families;*)
                          let child_local =
                            List.fold_left
                              (fun accu c -> (c, gen + 1) :: accu)
                              accu children
                          in
                          (* lien inter arbre *)
                          let family_link =
                            Perso_link.get_families_of_parents
                              conf.command (get_key_index p) (get_key_index sp)
                          in
                          let children_link =
                            List.fold_right
                              (fun fam_link accu ->
                                List.fold_right
                                  (fun c_link accu ->
                                    let baseprefix = c_link.MLink.Person_link.baseprefix in
                                    let ip_c =
                                      Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
                                    in
                                    match Perso_link.get_person_link baseprefix ip_c with
                                    | Some c_link ->
                                        let can_merge =
                                          Perso_link.can_merge_child conf.command
                                            (get_children fam) c_link
                                        in
                                        if can_merge then accu
                                        else c_link :: accu
                                    | None -> accu)
                                  fam_link.MLink.Family.children accu)
                              family_link []
                          in
                          nodes :=
                            List.fold_left
                              (fun nodes c_link ->
                                let baseprefix = c_link.MLink.Person.baseprefix in
                                let (c, _) = Perso_link.make_ep_link conf base c_link in
                                create_node c ifam gen Children baseprefix :: nodes)
                              !nodes children_link;
                          List.iter
                            (fun c_link ->
                              let (c, _) = Perso_link.make_ep_link conf base c_link in
                              edges := (create_edge p c) :: !edges;
                              edges := (create_edge sp c) :: !edges)
                            children_link;
                          let child_distant = [] in
                          child_local @ child_distant
                        end
                      else accu)
                    l (Array.to_list ifam)
                in
                let l_link =
                  let ip = get_key_index p in
                  let base_prefix = conf.command in
                  let families = Perso_link.get_family_link base_prefix ip in
                  List.fold_left
                    (fun accu fam_link ->
                       let (ifath, imoth, ifam) =
                         (Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.ifath),
                          Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.imoth),
                          Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam))
                       in
                       let cpl =
                         let ip = get_key_index p in
                         if ip <> ifath && ip <> imoth then
                           match
                             Perso_link.get_person_link_with_base
                               conf.command ip fam_link.MLink.Family.baseprefix
                           with
                           | Some p ->
                               let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
                               (ifath, imoth, if ip = ifath then imoth else ifath)
                           | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                         else (ifath, imoth, if ip = ifath then imoth else ifath)
                       in
                       let can_merge =
                         let fam = List.map (foi base) (Array.to_list (get_family p)) in
                         Perso_link.can_merge_family conf.command (get_key_index p) fam fam_link cpl
                       in
                       if can_merge then accu
                       else
                         let (_, _, isp) = cpl in
                         match Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp with
                         | Some sp ->
                             let (sp, _) = Perso_link.make_ep_link conf base sp in
                             let baseprefix = fam_link.MLink.Family.baseprefix in
                             let ifam = Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam) in
                             nodes := (create_node sp ifam gen Spouse baseprefix) :: !nodes;
                             edges := (create_edge p sp) :: !edges;
                             if gen <> max_gen then
                               begin
                                 let family_link =
                                   Perso_link.get_families_of_parents baseprefix ifath imoth
                                 in
                                 let children_link =
                                   List.fold_right
                                     (fun fam_link accu ->
                                       List.fold_right
                                         (fun c_link accu ->
                                           let baseprefix = c_link.MLink.Person_link.baseprefix in
                                           let ip_c =
                                             Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
                                           in
                                           match Perso_link.get_person_link baseprefix ip_c with
                                           | Some c_link -> c_link :: accu
                                           | None -> accu)
                                         fam_link.MLink.Family.children accu)
                                     family_link []
                                 in
                                 nodes :=
                                   List.fold_left
                                   (fun nodes c_link ->
                                     let baseprefix = c_link.MLink.Person.baseprefix in
                                     let (c, _) = Perso_link.make_ep_link conf base c_link in
                                     create_node c ifam gen Children baseprefix :: nodes)
                                   !nodes children_link;
                                 List.iter
                                   (fun c_link ->
                                     let (c, _) = Perso_link.make_ep_link conf base c_link in
                                     edges := (create_edge p c) :: !edges;
                                     edges := (create_edge sp c) :: !edges)
                                   children_link;
                                 accu
                               end
                             else accu
                         | None -> accu)
                    [] families
                in
                let l = l @ l_link in
                loop l nodes edges families
              end
          end
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  nodes := create_node p (Adef.ifam_of_int (-1)) 1 Root conf.command :: !nodes;
  loop [(p, 1)] nodes edges families
;;


(* ********************************************************************* *)
(*  [Fonc] print_tree : conf -> base -> todo                             *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] :
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_graph_tree conf base =
  let params = get_params conf Mext_read.parse_graph_tree_params in
  let ip = Adef.iper_of_int (Int32.to_int params.Mread.Graph_tree_params.index) in
  let () = Perso.build_sosa_ht conf base in
  let p = poi base ip in
  let max_asc = 12 in
  let nb_asc =
    match params.Mread.Graph_tree_params.nb_asc with
    | Some n -> min max_asc (max (Int32.to_int n) 1)
    | None -> max_asc
  in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let (nodes_asc, edges_asc, _) = build_graph_asc conf base p nb_asc false in
  (*
  let nodes_asc =
    List.rev_map
      (fun p ->
        let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
        let p = pers_to_piqi_person_tree conf base p in
        Mread.Node.({
          id = id;
          person = p;
          ifam = None;
        })
      nodes_asc
  in
  *)
  let max_desc = 12 in
  let nb_desc =
    match params.Mread.Graph_tree_params.nb_desc with
    | Some n -> min max_desc (max (Int32.to_int n) 1)
    | None -> max_desc
  in
  let (nodes_desc, edges_desc, _) =
    build_graph_desc conf base p nb_desc false
  in
  let nodes_siblings =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        List.fold_right
          (fun c accu ->
            if c = ip then accu
            else
              let c = poi base c in
              let id = Int64.of_int (Adef.int_of_iper (get_key_index c)) in
              let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
              let node =
                Mread.Node.({
                  id = id;
                  person = c;
                  ifam = None;
                })
              in
              node :: accu)
          (Array.to_list (get_children fam)) []
    | None -> []
  in
  let graph =
    Mread.Graph_tree.({
      nodes_asc = nodes_asc;
      edges_asc = edges_asc;
      nodes_desc = nodes_desc;
      edges_desc = edges_desc;
      nodes_siblings = nodes_siblings;
    })
  in
  let data = Mext_read.gen_graph_tree graph in
  print_result conf data
;;


(**/**) (* V2 *)

(* Graphe d'ascendance v2 *)

let build_graph_asc_v2 conf base p max_gen =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_int (Hashtbl.hash (baseprefix_from, get_key_index p_from, factor_from))
    in
    let id_to =
      Int64.of_int (Hashtbl.hash (baseprefix_to, get_key_index p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_key_index p, factor) in
    let id = Int64.of_int uniq_id in
    let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
    Mread.Node.({
      id = id;
      person = p;
      ifam = None;
    })
  in
(*
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
*)
  let nodes = ref [] in
  let edges = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            match get_parents p with
            | Some ifam ->
                let factor =
                  try Hashtbl.find ht (get_key_index p) with Not_found -> 1
                in
                let cpl = foi base ifam in
                let fath = poi base (get_father cpl) in
                let moth = poi base (get_mother cpl) in
                let fath_factor =
                  try
                    let i = Hashtbl.find ht (get_key_index fath) + 1 in
                    Hashtbl.replace ht (get_key_index fath) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_key_index fath) 1; 1
                in
                let moth_factor =
                  try
                    let i = Hashtbl.find ht (get_key_index moth) + 1 in
                    Hashtbl.replace ht (get_key_index moth) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_key_index moth) 1; 1
                in
                nodes := create_node fath gen Ancestor conf.command fath_factor :: !nodes;
                nodes := create_node moth gen Ancestor conf.command moth_factor :: !nodes;
                edges := create_edge factor conf.command p fath_factor conf.command fath :: !edges;
                edges := create_edge factor conf.command p moth_factor conf.command moth :: !edges;
                (*create_family ifam families;*)
                loop ((fath, gen + 1) :: (moth, gen + 1) :: l)
            | None ->
                (* lien inter arbre *)
                let ip = get_key_index p in
                let () =
                  Perso_link.init_cache conf base ip (max_gen - gen) 0 0
                in
                let () =
                  let ht = Hashtbl.create 42 in
                  let rec loop_parents l =
                    match l with
                    | [] -> ()
                    | (base_prefix, p, gen) :: l ->
                        if gen >= max_gen then loop_parents l
                        else
                          let factor =
                            try Hashtbl.find ht (base_prefix, get_key_index p) with Not_found -> 1
                          in
                          let ip = get_key_index p in
                          match Perso_link.get_parents_link base_prefix ip with
                          | Some family ->
                              begin
                                let ifath = Adef.iper_of_int (Int32.to_int family.MLink.Family.ifath) in
                                let imoth = Adef.iper_of_int (Int32.to_int family.MLink.Family.imoth) in
                                let fam_base_prefix = family.MLink.Family.baseprefix in
                                match
                                  (Perso_link.get_person_link fam_base_prefix ifath,
                                   Perso_link.get_person_link fam_base_prefix imoth,
                                   Perso_link.get_person_link base_prefix ip)
                                with
                                | (Some pfath, Some pmoth, Some c) ->
                                    let (fath, _) = Perso_link.make_ep_link conf base pfath in
                                    let (moth, _) = Perso_link.make_ep_link conf base pmoth in
                                    let fath_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_key_index fath) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_key_index fath) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_key_index fath) 1; 1
                                    in
                                    let moth_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_key_index moth) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_key_index moth) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_key_index moth) 1; 1
                                    in
                                    nodes := create_node fath gen Ancestor pfath.MLink.Person.baseprefix fath_factor :: !nodes;
                                    nodes := create_node moth gen Ancestor pmoth.MLink.Person.baseprefix moth_factor :: !nodes;
                                    edges := create_edge factor base_prefix p fath_factor pfath.MLink.Person.baseprefix fath :: !edges;
                                    edges := create_edge factor base_prefix p moth_factor pmoth.MLink.Person.baseprefix moth :: !edges;
                                    let l =
                                      ((fam_base_prefix, fath, gen + 1) :: (fam_base_prefix, moth, gen + 1) :: l)
                                    in
                                    loop_parents l
                                | _ -> loop_parents l
                              end
                          | None -> loop_parents l
                  in
                  loop_parents [(conf.command, p, gen)]
                in
                loop l
          end
  in
  nodes := create_node p 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges)
;;


(* Graphe de descendance v2 *)

let build_graph_desc_v2 conf base p max_gen =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_int (Hashtbl.hash (baseprefix_from, get_key_index p_from, factor_from))
    in
    let id_to =
      Int64.of_int (Hashtbl.hash (baseprefix_to, get_key_index p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p ifam gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_key_index p, factor) in
    let id = Int64.of_int uniq_id in
    let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
    let ifam = Int64.of_int (Adef.int_of_ifam ifam) in
    Mread.Node.({
      id = id;
      person = p;
      ifam = Some ifam;
    })
  in
(*
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family conf base ifam) :: !families
  in
*)
  let nodes = ref [] in
  let edges = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            let factor =
              try Hashtbl.find ht (get_key_index p) with Not_found -> 1
            in
            let ifam = get_family p in
            let l =
              List.fold_left
                (fun accu ifam  ->
                  let fam = foi base ifam in
                  let sp = poi base (Gutil.spouse (get_key_index p) fam) in
                  let sp_factor =
                    try
                      let i = Hashtbl.find ht (get_key_index sp) + 1 in
                      Hashtbl.replace ht (get_key_index sp) i;
                      i
                    with Not_found -> Hashtbl.add ht (get_key_index sp) 1; 1
                  in
                  let children =
                    List.map (poi base) (Array.to_list (get_children fam))
                  in
                  nodes := create_node sp ifam gen Spouse conf.command sp_factor :: !nodes;
                  edges := create_edge factor conf.command p sp_factor conf.command sp :: !edges;
                  if gen <> max_gen then
                    begin
                      List.iter
                        (fun c ->
                          let c_factor =
                            try
                              let i = Hashtbl.find ht (get_key_index c) + 1 in
                              Hashtbl.replace ht (get_key_index c) i;
                              i
                            with Not_found -> Hashtbl.add ht (get_key_index c) 1; 1
                          in
                          nodes := create_node c ifam gen Children conf.command c_factor :: !nodes;
                          edges := create_edge factor conf.command p c_factor conf.command c :: !edges;
                          edges := create_edge sp_factor conf.command sp c_factor conf.command c :: !edges)
                        children;
                      (*create_family ifam families;*)
                      let child_local =
                        List.fold_left
                          (fun accu c -> (c, gen + 1) :: accu)
                          accu children
                      in

                      (* lien inter arbre *)
                      let () =
                        Perso_link.init_cache conf base (get_key_index p) 1 1 (max_gen - gen)
                      in
                      let () =
                        let ht = Hashtbl.create 42 in
                        let rec loop_child fam_link =
                          match fam_link with
                          | [] -> ()
                          | (base_prefix, p, gen) :: l ->
                              if gen >= max_gen then loop_child l
                              else
                                begin
                                  let factor =
                                    try Hashtbl.find ht (base_prefix, get_key_index p) with Not_found -> 1
                                  in
                                  let family_link =
                                    Perso_link.get_families_of_parents
                                      base_prefix (get_key_index p) (get_key_index sp)
                                  in
                                  let children_link =
                                    List.fold_left
                                      (fun accu fam_link ->
                                        let (ifath, imoth, ifam) =
                                          (Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.ifath),
                                           Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.imoth),
                                           Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam))
                                        in
                                        let cpl =
                                          let ip = get_key_index p in
                                          if ip <> ifath && ip <> imoth then
                                          match
                                            Perso_link.get_person_link_with_base
                                              conf.command ip fam_link.MLink.Family.baseprefix
                                          with
                                          | Some p ->
                                              let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
                                              (ifath, imoth, if ip = ifath then imoth else ifath)
                                          | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                          else (ifath, imoth, if ip = ifath then imoth else ifath)
                                        in
                                        let (_, _, isp) = cpl in
                                        let sp_factor =
                                          try
                                            let i = Hashtbl.find ht (fam_link.MLink.Family.baseprefix, isp) + 1 in
                                            Hashtbl.replace ht (fam_link.MLink.Family.baseprefix, isp) i;
                                            i
                                          with Not_found -> Hashtbl.add ht (fam_link.MLink.Family.baseprefix, isp) 1; 1
                                        in
                                        List.fold_left
                                          (fun accu c_link ->
                                            let baseprefix = c_link.MLink.Person_link.baseprefix in
                                            let ip_c =
                                              Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
                                            in
                                            match Perso_link.get_person_link baseprefix ip_c with
                                            | Some c_link ->
                                                let can_merge =
                                                  Perso_link.can_merge_child base_prefix
                                                    (get_children fam) c_link
                                                in
                                                if can_merge then accu
                                                else
                                                  let (c, _) = Perso_link.make_ep_link conf base c_link in
                                                  let c_factor =
                                                    try
                                                      let i = Hashtbl.find ht (baseprefix, get_key_index c) + 1 in
                                                      Hashtbl.replace ht (baseprefix, get_key_index c) i;
                                                      i
                                                    with Not_found -> Hashtbl.add ht (baseprefix, get_key_index c) 1; 1
                                                  in
                                                  nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                  edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                  edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                  (baseprefix, c, gen + 1) :: accu
                                            | None -> accu)
                                          accu fam_link.MLink.Family.children)
                                      l family_link
                                  in
                                  loop_child children_link;
                                end
                        in
                        loop_child [(conf.command, p, gen)]
                      in
                      child_local
                    end
                  else accu)
                l (Array.to_list ifam)
            in

            (* lien inter arbre *)
            let () =
              Perso_link.init_cache conf base (get_key_index p) 1 1 (max_gen - gen)
            in
            let () =
              let ht = Hashtbl.create 42 in
              let rec loop_desc l =
                match l with
                | [] -> ()
                | (base_prefix, p, gen) :: l ->
                    if gen >= max_gen then loop_desc l
                    else
                      begin
                        let ip = get_key_index p in
                        let families = Perso_link.get_family_link base_prefix ip in
                        let l =
                          List.fold_left
                            (fun accu fam_link ->
                               let (ifath, imoth, ifam) =
                                 (Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.ifath),
                                  Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.imoth),
                                  Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam))
                               in
                               let cpl =
                                 let ip = get_key_index p in
                                 if ip <> ifath && ip <> imoth then
                                   match
                                     Perso_link.get_person_link_with_base
                                       conf.command ip fam_link.MLink.Family.baseprefix
                                   with
                                   | Some p ->
                                       let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
                                       (ifath, imoth, if ip = ifath then imoth else ifath)
                                   | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                 else (ifath, imoth, if ip = ifath then imoth else ifath)
                               in
                               let can_merge =
                                 let fam = List.map (foi base) (Array.to_list (get_family p)) in
                                 Perso_link.can_merge_family conf.command (get_key_index p) fam fam_link cpl
                               in
                               if can_merge then accu
                               else
                                 let (_, _, isp) = cpl in
                                 match Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp with
                                 | Some sp ->
                                     let (sp, _) = Perso_link.make_ep_link conf base sp in
                                     let baseprefix = fam_link.MLink.Family.baseprefix in
                                     let ifam = Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam) in
                                     let sp_factor =
                                       try
                                         let i = Hashtbl.find ht (baseprefix, get_key_index sp) + 1 in
                                         Hashtbl.replace ht (baseprefix, get_key_index sp) i;
                                         i
                                       with Not_found -> Hashtbl.add ht (baseprefix, get_key_index sp) 1; 1
                                     in
                                     nodes := create_node sp ifam gen Spouse baseprefix sp_factor :: !nodes;
                                     edges := create_edge factor base_prefix p sp_factor baseprefix sp :: !edges;
                                     if gen <> max_gen then
                                       begin
                                         let family_link =
                                           Perso_link.get_families_of_parents baseprefix ifath imoth
                                         in
                                         let children_link =
                                           List.fold_left
                                             (fun accu fam_link ->
                                               List.fold_left
                                                 (fun accu c_link ->
                                                   let baseprefix = c_link.MLink.Person_link.baseprefix in
                                                   let ip_c =
                                                     Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
                                                   in
                                                   match Perso_link.get_person_link baseprefix ip_c with
                                                   | Some c_link ->
                                                       let (c, _) = Perso_link.make_ep_link conf base c_link in
                                                       let c_factor =
                                                         try
                                                           let i = Hashtbl.find ht (baseprefix, get_key_index c) + 1 in
                                                           Hashtbl.replace ht (baseprefix, get_key_index c) i;
                                                           i
                                                         with Not_found -> Hashtbl.add ht (baseprefix, get_key_index c) 1; 1
                                                       in
                                                       nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                       edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                       edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                       (baseprefix, c, gen + 1) :: accu
                                                   | None -> accu)
                                                 accu fam_link.MLink.Family.children)
                                             accu family_link
                                         in
                                         children_link
                                       end
                                     else accu
                                 | None -> accu)
                            l families
                        in
                        loop_desc l
                      end
              in
              loop_desc [(conf.command, p, gen)]
            in

            loop l
          end
  in
  nodes := create_node p (Adef.ifam_of_int (-1)) 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges)
;;


(* ********************************************************************* *)
(*  [Fonc] print_tree_v2 : conf -> base -> todo                          *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] :
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_graph_tree_v2 conf base =
  let params = get_params conf Mext_read.parse_graph_tree_params in
  let ip = Adef.iper_of_int (Int32.to_int params.Mread.Graph_tree_params.index) in
  let () = Perso.build_sosa_ht conf base in
  let p = poi base ip in
  let max_asc = 12 in
  let nb_asc =
    match params.Mread.Graph_tree_params.nb_asc with
    | Some n -> min max_asc (max (Int32.to_int n) 1)
    | None -> max_asc
  in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let (nodes_asc, edges_asc) = build_graph_asc_v2 conf base p nb_asc in
  (*
  let nodes_asc =
    List.rev_map
      (fun p ->
        let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
        let p = pers_to_piqi_person_tree conf base p in
        Mread.Node.({
          id = id;
          person = p;
          ifam = None;
        }))
      nodes_asc
  in
  *)
  let max_desc = 12 in
  let nb_desc =
    match params.Mread.Graph_tree_params.nb_desc with
    | Some n -> min max_desc (max (Int32.to_int n) 1)
    | None -> max_desc
  in
  let (nodes_desc, edges_desc) = build_graph_desc_v2 conf base p nb_desc in
  let nodes_siblings =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        List.fold_right
          (fun ic accu ->
            if ic = ip then accu
            else
              let c = poi base ic in
              (* Pour les liens inter arbres, on rend l'id unique avec *)
              (* le prefix de la base et l'index de la personne.       *)
              let uniq_id = Hashtbl.hash (conf.command, ic) in
              let id = Int64.of_int uniq_id in
              let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
              let node =
                Mread.Node.({
                  id = id;
                  person = c;
                  ifam = None;
                })
              in
              node :: accu)
          (Array.to_list (get_children fam)) []
    | None -> []
  in
  let (nodes_siblings_before, nodes_siblings_after) =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        let children = Array.to_list (get_children fam) in
        let rec split_at_person before after l =
          match l with
          | [] -> (List.rev before, after)
          | ic :: l ->
              if ic = ip then
                let after =
                  List.map
                    (fun ic ->
                      let c = poi base ic in
                      (* Pour les liens inter arbres, on rend l'id unique avec *)
                      (* le prefix de la base et l'index de la personne.       *)
                      let uniq_id = Hashtbl.hash (conf.command, ic) in
                      let id = Int64.of_int uniq_id in
                      let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
                      Mread.Node.({
                        id = id;
                        person = c;
                        ifam = None;
                      }))
                    l
                in
                (List.rev before, after)
              else
                let c = poi base ic in
                (* Pour les liens inter arbres, on rend l'id unique avec *)
                (* le prefix de la base et l'index de la personne.       *)
                let uniq_id = Hashtbl.hash (conf.command, ic) in
                let id = Int64.of_int uniq_id in
                let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.command in
                let node =
                  Mread.Node.({
                    id = id;
                    person = c;
                    ifam = None;
                  })
                in
                split_at_person (node :: before) after l
        in
        split_at_person [] [] children
    | None -> ([], [])
  in
  let graph =
    Mread.Graph_tree_new.({
      nodes_asc = nodes_asc;
      edges_asc = edges_asc;
      nodes_desc = nodes_desc;
      edges_desc = edges_desc;
      nodes_siblings = nodes_siblings;
      nodes_siblings_before = nodes_siblings_before;
      nodes_siblings_after = nodes_siblings_after;
    })
  in
  let data = Mext_read.gen_graph_tree_new graph in
  print_result conf data
;;


(**/**) (* Version identique mais avec les infos "complète" d'une personne. *)


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_tree_full :
             config -> base -> person -> PersonTreeFull                       *)
(** [Description] : Retourne à partir d'une person (gwdb) une
                    PersonTreeFull (piqi)
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_tree_full conf base p more_info gen max_gen base_prefix =
  if is_restricted conf base (get_key_index p) then
    Mread.Person_tree_full.({
      index = Int32.of_int (-1);
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      image = None;
      sosa = `no_sosa;
      public_name = None;
      aliases = [];
      qualifiers = [];
      firstname_aliases = [];
      surname_aliases = [];
      birth_date = None;
      birth_place = None;
      birth_src = None;
      baptism_date = None;
      baptism_place = None;
      baptism_src = None;
      death_date = None;
      death_place = None;
      death_src = None;
      death_type = `dont_know_if_dead;
      burial_date = None;
      burial_place = None;
      burial_src = None;
      occupation = None;
      psources = None;
      titles = [];
      visible_for_visitors = false;
      has_more_infos = false;
      baseprefix = "";
    })
  else
    let gen_p = Util.string_gen_person base (gen_person_of_person p) in
    let p_auth = authorized_age conf base p in
    let index = Int32.of_int (Adef.int_of_iper gen_p.key_index) in
    let sex =
      match gen_p.sex with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      if conf.bname <> Link.chop_base_prefix base_prefix then `no_sosa
      else
        let sosa_nb = Perso.get_sosa_person conf base p in
        if Sosa.eq sosa_nb Sosa.zero then `no_sosa
        else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
        else `sosa
    in
    let sn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower gen_p.surname
    in
    let fn =
      if (is_hide_names conf p) && not p_auth then ""
      else Name.lower gen_p.first_name
    in
    let occ = Int32.of_int (get_occ p) in
    let (first_name, surname) =
      if not p_auth && (is_hide_names conf p) then ("x", "x")
      else person_firstname_surname_txt conf base p
    in
    let image =
      if not conf.no_image && p_auth then
        let img = gen_p.image in
        if img <> "" then img
        else
          match Api_util.find_image_file conf base p with
          | Some s -> "1"
          | None -> ""
      else ""
    in
    let public_name =
      if gen_p.public_name = "" then None
      else Some gen_p.public_name
    in
    let aliases = gen_p.aliases in
    let qualifiers = gen_p.qualifiers in
    let firstname_aliases = gen_p.first_names_aliases in
    let surname_aliases = gen_p.surnames_aliases in
    let birth =
      match Adef.od_of_codate gen_p.birth with
      | Some d when p_auth -> Some (string_of_date conf d)
      | _ -> None
    in
    let birth_place =
      if p_auth then Some gen_p.birth_place
      else None
    in
    let birth_src =
      if p_auth then Some gen_p.birth_src
      else None
    in
    let baptism =
      match Adef.od_of_codate gen_p.baptism with
      | Some d when p_auth -> Some (string_of_date conf d)
      | _ -> None
    in
    let baptism_place =
      if p_auth then Some gen_p.baptism_place
      else None
    in
    let baptism_src =
      if p_auth then Some gen_p.baptism_src
      else None
    in
    let (death_type, death) =
      if p_auth then
        match gen_p.death with
        | NotDead -> (`not_dead, None)
        | Death (_, cd) ->
            let d = Adef.date_of_cdate cd in
            (`dead, Some (string_of_date conf d))
        | DeadYoung -> (`dead_young, None)
        | DeadDontKnowWhen -> (`dead_dont_know_when, None)
        | DontKnowIfDead -> (`dont_know_if_dead, None)
        | OfCourseDead -> (`of_course_dead, None)
      else
        (`not_dead, None)
    in
    let death_place =
      if p_auth then Some gen_p.death_place
      else None
    in
    let death_src =
      if p_auth then Some gen_p.death_src
      else None
    in
    let burial =
      match get_burial p with
      | Buried cod | Cremated cod ->
          (match Adef.od_of_codate cod with
          | Some d when p_auth -> Some (string_of_date conf d)
          | _ -> None)
      | _ -> None
    in
    let burial_place =
      if p_auth then Some gen_p.burial_place
      else None
    in
    let burial_src =
      if p_auth then Some gen_p.burial_src
      else None
    in
    let titles =
      List.map
        (fun t ->
          let (title_type, name) =
            match t.t_name with
            | Tmain -> (`title_main, "")
            | Tname name -> (`title_name, name)
            | Tnone -> (`title_none, "")
          in
          let title = t.t_ident in
          let fief = t.t_place in
          let date_begin =
            match Adef.od_of_codate t.t_date_start with
            | Some d -> Some (string_of_date conf d)
            | None -> None
          in
          let date_end =
            match Adef.od_of_codate t.t_date_end with
            | Some d -> Some (string_of_date conf d)
            | None -> None
          in
          let nth = Some (Int32.of_int t.t_nth) in
          Mread.Title.({
            title_type = title_type;
            name = if name = "" then None else Some name;
            title = if title = "" then None else Some title;
            fief = if fief = "" then None else Some fief;
            date_begin = date_begin;
            date_end = date_end;
            nth = nth;
          }))
        gen_p.titles
    in
    let occupation =
      if p_auth then Some gen_p.occupation
      else None
    in
    let psources =
      if p_auth then Some gen_p.psources
      else None
    in
    let visible = is_visible conf base p in
    let has_more_infos =
      match more_info with
      | Root -> false
      | Siblings -> Array.length (get_family p) > 0
      | Children ->
           gen = max_gen - 1 && Array.length (get_family p) > 0
           (*
           fst (List.fold_left
                  (fun (children_or_spouses, nb_fam) ifam ->
                    let nb_fam = succ nb_fam in
                    let fam = foi base ifam in
                    let children = get_children fam in
                    (children_or_spouses || Array.length children > 1 ||
                       nb_fam > 1, nb_fam))
                  (false, 0) (Array.to_list (get_family p)))
           *)
      | Ancestor ->
          let has_parents =
            match get_parents p with
            | Some ifam -> true
            | _ -> false
          in
          (gen = max_gen - 1 && has_parents) ||
           (fst (List.fold_left
                   (fun (children_or_spouses, nb_fam) ifam ->
                     let nb_fam = succ nb_fam in
                     let fam = foi base ifam in
                     let children = get_children fam in
                     (children_or_spouses ||
                      (gen > 1 && Array.length children > 1) ||
                      nb_fam > 1,
                      nb_fam))
                   (false, 0) (Array.to_list (get_family p))))
      | Spouse ->
          let has_parents =
            match get_parents p with
            | Some ifam -> true
            | _ -> false
          in
          has_parents || Array.length (get_family p) > 1
    in
    Mread.Person_tree_full.({
      index = index;
      sex = sex;
      lastname = surname;
      firstname = first_name;
      n = sn;
      p = fn;
      occ = occ;
      image = if image = "" then None else Some image;
      sosa = sosa;
      public_name = public_name;
      aliases = aliases;
      qualifiers = qualifiers;
      firstname_aliases = firstname_aliases;
      surname_aliases = surname_aliases;
      birth_date = birth;
      birth_place = birth_place;
      birth_src = birth_src;
      baptism_date = baptism;
      baptism_place = baptism_place;
      baptism_src = baptism_src;
      death_date = death;
      death_place = death_place;
      death_src = death_src;
      death_type = death_type;
      burial_date = burial;
      burial_place = burial_place;
      burial_src = burial_src;
      occupation = occupation;
      psources = psources;
      titles = titles;
      visible_for_visitors = visible;
      has_more_infos = has_more_infos;
      baseprefix = base_prefix
    })
;;


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family_tree :
             config -> base -> ifam -> Full_family                       *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family_tree conf base ifam =
  let fam = foi base ifam in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let m_auth =
    authorized_age conf base (pget conf base ifath) &&
    authorized_age conf base (pget conf base imoth)
  in
  let index = Int32.of_int (Adef.int_of_ifam ifam) in
  let fsources =
    if m_auth then Some gen_f.fsources
    else None
  in
  let marriage =
    match Adef.od_of_codate gen_f.marriage with
    | Some d when m_auth -> Some (string_of_date conf d)
    | _ -> None
  in
  let marriage_place =
    if m_auth then Some gen_f.marriage_place
    else None
  in
  let marriage_src =
    if m_auth then Some gen_f.marriage_src
    else None
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Adef.od_of_codate cod with
         | Some d when m_auth -> (`divorced, Some (string_of_date conf d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  Mread.Family_tree_full.({
    fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    index = index;
  })
;;


(* *********************************************************************** *)
(*  [Fonc] fam_to_piqi_family_tree_link :
             config -> base -> ifam -> Full_family                         *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let fam_to_piqi_family_tree_link conf base (ifath, imoth) ifam fam =
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let index = Int32.of_int (Adef.int_of_ifam ifam) in
  let fsources = None in
  let marriage =
    match Adef.od_of_codate gen_f.marriage with
    | Some d -> Some (string_of_date conf d)
    | _ -> None
  in
  let marriage_place = Some gen_f.marriage_place in
  let marriage_src = Some gen_f.marriage_src in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Adef.od_of_codate cod with
         | Some d -> (`divorced, Some (string_of_date conf d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  Mread.Family_tree_full.({
    fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    index = index;
  })
;;


(* Graphe d'ascendance (api.proto) *)

let build_graph_asc_full conf base p max_gen =
(*
  let () = load_ascends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_int (Hashtbl.hash (baseprefix_from, get_key_index p_from, factor_from))
    in
    let id_to =
      Int64.of_int (Hashtbl.hash (baseprefix_to, get_key_index p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_key_index p, factor) in
    let id = Int64.of_int uniq_id in
    let p = pers_to_piqi_person_tree_full conf base p more_info gen max_gen base_prefix in
    Mread.Node_full.({
      id = id;
      person = p;
      ifam = None;
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family_tree conf base ifam) :: !families
  in
  let create_family_link (ifath, imoth) ifam fam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families :=
        (fam_to_piqi_family_tree_link conf base (ifath, imoth) ifam fam) :: !families
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            match get_parents p with
            | Some ifam ->
                let factor =
                  try Hashtbl.find ht (get_key_index p) with Not_found -> 1
                in
                let cpl = foi base ifam in
                let fath = poi base (get_father cpl) in
                let moth = poi base (get_mother cpl) in
                let fath_factor =
                  try
                    let i = Hashtbl.find ht (get_key_index fath) + 1 in
                    Hashtbl.replace ht (get_key_index fath) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_key_index fath) 1; 1
                in
                let moth_factor =
                  try
                    let i = Hashtbl.find ht (get_key_index moth) + 1 in
                    Hashtbl.replace ht (get_key_index moth) i;
                    i
                  with Not_found -> Hashtbl.add ht (get_key_index moth) 1; 1
                in
                nodes := create_node fath gen Ancestor conf.command fath_factor :: !nodes;
                nodes := create_node moth gen Ancestor conf.command moth_factor :: !nodes;
                edges := create_edge factor conf.command p fath_factor conf.command fath :: !edges;
                edges := create_edge factor conf.command p moth_factor conf.command moth :: !edges;
                create_family ifam families;
                loop ((fath, gen + 1) :: (moth, gen + 1) :: l)
            | None ->
                (* lien inter arbre *)
                let ip = get_key_index p in
                let () =
                  Perso_link.init_cache conf base ip (max_gen - gen) 0 0
                in
                let () =
                  let ht = Hashtbl.create 42 in
                  let rec loop_parents l =
                    match l with
                    | [] -> ()
                    | (base_prefix, p, gen) :: l ->
                        if gen >= max_gen then loop_parents l
                        else
                          let factor =
                            try Hashtbl.find ht (base_prefix, get_key_index p) with Not_found -> 1
                          in
                          let ip = get_key_index p in
                          match Perso_link.get_parents_link base_prefix ip with
                          | Some family ->
                              begin
                                let ifath = Adef.iper_of_int (Int32.to_int family.MLink.Family.ifath) in
                                let imoth = Adef.iper_of_int (Int32.to_int family.MLink.Family.imoth) in
                                let fam_base_prefix = family.MLink.Family.baseprefix in
                                let (ifam, fam, _, _) = Perso_link.make_efam_link conf base ifath family in
                                match
                                  (Perso_link.get_person_link fam_base_prefix ifath,
                                   Perso_link.get_person_link fam_base_prefix imoth,
                                   Perso_link.get_person_link base_prefix ip)
                                with
                                | (Some pfath, Some pmoth, Some c) ->
                                    let (fath, _) = Perso_link.make_ep_link conf base pfath in
                                    let (moth, _) = Perso_link.make_ep_link conf base pmoth in
                                    let fath_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_key_index fath) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_key_index fath) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_key_index fath) 1; 1
                                    in
                                    let moth_factor =
                                      try
                                        let i = Hashtbl.find ht (fam_base_prefix, get_key_index moth) + 1 in
                                        Hashtbl.replace ht (fam_base_prefix, get_key_index moth) i;
                                        i
                                      with Not_found -> Hashtbl.add ht (fam_base_prefix, get_key_index moth) 1; 1
                                    in
                                    nodes := create_node fath gen Ancestor pfath.MLink.Person.baseprefix fath_factor :: !nodes;
                                    nodes := create_node moth gen Ancestor pmoth.MLink.Person.baseprefix moth_factor :: !nodes;
                                    edges := create_edge factor base_prefix p fath_factor pfath.MLink.Person.baseprefix fath :: !edges;
                                    edges := create_edge factor base_prefix p moth_factor pmoth.MLink.Person.baseprefix moth :: !edges;
                                    create_family_link (ifath, imoth) ifam fam families;
                                    let l =
                                      ((fam_base_prefix, fath, gen + 1) :: (fam_base_prefix, moth, gen + 1) :: l)
                                    in
                                    loop_parents l
                                | _ -> loop_parents l
                              end
                          | None -> loop_parents l
                  in
                  loop_parents [(conf.command, p, gen)]
                in
                loop l
          end
  in
  nodes := create_node p 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges, List.rev !families)
;;


(* Graphe de descendance (api.proto) *)

let build_graph_desc_full conf base p max_gen =
(*
  let () = load_descends_array base in
  let () = load_unions_array base in
  let () = load_couples_array base in
  let () = Perso.build_sosa_ht conf base in
*)
  let ht = Hashtbl.create 42 in
  let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let id_from =
      Int64.of_int (Hashtbl.hash (baseprefix_from, get_key_index p_from, factor_from))
    in
    let id_to =
      Int64.of_int (Hashtbl.hash (baseprefix_to, get_key_index p_to, factor_to))
    in
    Mread.Edge.({
      from_node = id_from;
      to_node = id_to;
    })
  in
  let create_node p ifam gen more_info base_prefix factor =
    (* Pour les liens inter arbres, on rend l'id unique avec *)
    (* le prefix de la base et l'index de la personne.       *)
    let uniq_id = Hashtbl.hash (base_prefix, get_key_index p, factor) in
    let id = Int64.of_int uniq_id in
    let p = pers_to_piqi_person_tree_full conf base p more_info gen max_gen base_prefix in
    let ifam = Int64.of_int (Adef.int_of_ifam ifam) in
    Mread.Node_full.({
      id = id;
      person = p;
      ifam = Some ifam;
    })
  in
  let create_family ifam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families := (fam_to_piqi_family_tree conf base ifam) :: !families
  in
  let create_family_link (ifath, imoth) ifam fam families =
    if p_getenv conf.env "full_infos" = Some "1" then
      families :=
        (fam_to_piqi_family_tree_link conf base (ifath, imoth) ifam fam) :: !families
  in
  let nodes = ref [] in
  let edges = ref [] in
  let families = ref [] in
  let rec loop l =
    match l with
    | [] -> ()
    | (p, gen) :: l ->
        if gen >= max_gen then loop l
        else
          begin
            let factor =
              try Hashtbl.find ht (get_key_index p) with Not_found -> 1
            in
            let ifam = get_family p in
            let l =
              List.fold_left
                (fun accu ifam  ->
                  let fam = foi base ifam in
                  let sp = poi base (Gutil.spouse (get_key_index p) fam) in
                  let sp_factor =
                    try
                      let i = Hashtbl.find ht (get_key_index sp) + 1 in
                      Hashtbl.replace ht (get_key_index sp) i;
                      i
                    with Not_found -> Hashtbl.add ht (get_key_index sp) 1; 1
                  in
                  let children =
                    List.map (poi base) (Array.to_list (get_children fam))
                  in
                  nodes := create_node sp ifam gen Spouse conf.command sp_factor :: !nodes;
                  edges := create_edge factor conf.command p sp_factor conf.command sp :: !edges;
                  if gen <> max_gen then
                    begin
                      List.iter
                        (fun c ->
                          let c_factor =
                            try
                              let i = Hashtbl.find ht (get_key_index c) + 1 in
                              Hashtbl.replace ht (get_key_index c) i;
                              i
                            with Not_found -> Hashtbl.add ht (get_key_index c) 1; 1
                          in
                          nodes := create_node c ifam gen Children conf.command c_factor :: !nodes;
                          edges := create_edge factor conf.command p c_factor conf.command c :: !edges;
                          edges := create_edge sp_factor conf.command sp c_factor conf.command c :: !edges)
                        children;
                      create_family ifam families;
                      let child_local =
                        List.fold_left
                          (fun accu c -> (c, gen + 1) :: accu)
                          accu children
                      in

                      (* lien inter arbre *)
                      let () =
                        Perso_link.init_cache conf base (get_key_index p) 1 1 (max_gen - gen)
                      in
                      let () =
                        let ht = Hashtbl.create 42 in
                        let rec loop_child fam_link =
                          match fam_link with
                          | [] -> ()
                          | (base_prefix, p, gen) :: l ->
                              if gen >= max_gen then loop_child l
                              else
                                begin
                                  let factor =
                                    try Hashtbl.find ht (base_prefix, get_key_index p) with Not_found -> 1
                                  in
                                  let family_link =
                                    Perso_link.get_families_of_parents
                                      base_prefix (get_key_index p) (get_key_index sp)
                                  in
                                  let children_link =
                                    List.fold_left
                                      (fun accu fam_link ->
                                        let (ifath, imoth, ifam) =
                                          (Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.ifath),
                                           Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.imoth),
                                           Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam))
                                        in
                                        let cpl =
                                          let ip = get_key_index p in
                                          if ip <> ifath && ip <> imoth then
                                          match
                                            Perso_link.get_person_link_with_base
                                              conf.command ip fam_link.MLink.Family.baseprefix
                                          with
                                          | Some p ->
                                              let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
                                              (ifath, imoth, if ip = ifath then imoth else ifath)
                                          | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                          else (ifath, imoth, if ip = ifath then imoth else ifath)
                                        in
                                        let (_, _, isp) = cpl in
                                        let sp_factor =
                                          try
                                            let i = Hashtbl.find ht (fam_link.MLink.Family.baseprefix, isp) + 1 in
                                            Hashtbl.replace ht (fam_link.MLink.Family.baseprefix, isp) i;
                                            i
                                          with Not_found -> Hashtbl.add ht (fam_link.MLink.Family.baseprefix, isp) 1; 1
                                        in
                                        create_family_link (ifath, imoth) ifam fam families;
                                        List.fold_left
                                          (fun accu c_link ->
                                            let baseprefix = c_link.MLink.Person_link.baseprefix in
                                            let ip_c =
                                              Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
                                            in
                                            match Perso_link.get_person_link baseprefix ip_c with
                                            | Some c_link ->
                                                let can_merge =
                                                  Perso_link.can_merge_child base_prefix
                                                    (get_children fam) c_link
                                                in
                                                if can_merge then accu
                                                else
                                                  let (c, _) = Perso_link.make_ep_link conf base c_link in
                                                  let c_factor =
                                                    try
                                                      let i = Hashtbl.find ht (baseprefix, get_key_index c) + 1 in
                                                      Hashtbl.replace ht (baseprefix, get_key_index c) i;
                                                      i
                                                    with Not_found -> Hashtbl.add ht (baseprefix, get_key_index c) 1; 1
                                                  in
                                                  nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                  edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                  edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                  (baseprefix, c, gen + 1) :: accu
                                            | None -> accu)
                                          accu fam_link.MLink.Family.children)
                                      l family_link
                                  in
                                  loop_child children_link;
                                end
                        in
                        loop_child [(conf.command, p, gen)]
                      in
                      child_local
                    end
                  else accu)
                l (Array.to_list ifam)
            in

            (* lien inter arbre *)
            let () =
              Perso_link.init_cache conf base (get_key_index p) 1 1 (max_gen - gen)
            in
            let () =
              let ht = Hashtbl.create 42 in
              let rec loop_desc l =
                match l with
                | [] -> ()
                | (base_prefix, p, gen) :: l ->
                    if gen >= max_gen then loop_desc l
                    else
                      begin
                        let ip = get_key_index p in
                        let families = Perso_link.get_family_link base_prefix ip in
                        let l =
                          List.fold_left
                            (fun accu fam_link ->
                               let (ifath, imoth, ifam) =
                                 (Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.ifath),
                                  Adef.iper_of_int (Int32.to_int fam_link.MLink.Family.imoth),
                                  Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam))
                               in
                               let cpl =
                                 let ip = get_key_index p in
                                 if ip <> ifath && ip <> imoth then
                                   match
                                     Perso_link.get_person_link_with_base
                                       conf.command ip fam_link.MLink.Family.baseprefix
                                   with
                                   | Some p ->
                                       let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
                                       (ifath, imoth, if ip = ifath then imoth else ifath)
                                   | None -> (ifath, imoth, if ip = ifath then imoth else ifath)
                                 else (ifath, imoth, if ip = ifath then imoth else ifath)
                               in
                               let can_merge =
                                 let fam = List.map (foi base) (Array.to_list (get_family p)) in
                                 Perso_link.can_merge_family conf.command (get_key_index p) fam fam_link cpl
                               in
                               if can_merge then accu
                               else
                                 let (_, _, isp) = cpl in
                                 match Perso_link.get_person_link fam_link.MLink.Family.baseprefix isp with
                                 | Some sp ->
                                     let (sp, _) = Perso_link.make_ep_link conf base sp in
                                     let baseprefix = fam_link.MLink.Family.baseprefix in
                                     let ifam = Adef.ifam_of_int (Int32.to_int fam_link.MLink.Family.ifam) in
                                     let sp_factor =
                                       try
                                         let i = Hashtbl.find ht (baseprefix, get_key_index sp) + 1 in
                                         Hashtbl.replace ht (baseprefix, get_key_index sp) i;
                                         i
                                       with Not_found -> Hashtbl.add ht (baseprefix, get_key_index sp) 1; 1
                                     in
                                     nodes := create_node sp ifam gen Spouse baseprefix sp_factor :: !nodes;
                                     edges := create_edge factor base_prefix p sp_factor baseprefix sp :: !edges;
                                     if gen <> max_gen then
                                       begin
                                         let family_link =
                                           Perso_link.get_families_of_parents baseprefix ifath imoth
                                         in
                                         (* TODO ?
                                         let (_, fam, _, _) =
                                           Perso_link.make_efam_link conf base ip fam_link
                                         in
                                         create_family_link (ifath, imoth) ifam fam families;
                                         *)
                                         let children_link =
                                           List.fold_left
                                             (fun accu fam_link ->
                                               List.fold_left
                                                 (fun accu c_link ->
                                                   let baseprefix = c_link.MLink.Person_link.baseprefix in
                                                   let ip_c =
                                                     Adef.iper_of_int (Int32.to_int c_link.MLink.Person_link.ip)
                                                   in
                                                   match Perso_link.get_person_link baseprefix ip_c with
                                                   | Some c_link ->
                                                       let (c, _) = Perso_link.make_ep_link conf base c_link in
                                                       let c_factor =
                                                         try
                                                           let i = Hashtbl.find ht (baseprefix, get_key_index c) + 1 in
                                                           Hashtbl.replace ht (baseprefix, get_key_index c) i;
                                                           i
                                                         with Not_found -> Hashtbl.add ht (baseprefix, get_key_index c) 1; 1
                                                       in
                                                       nodes := create_node c ifam gen Children baseprefix c_factor :: !nodes;
                                                       edges := create_edge factor base_prefix p c_factor baseprefix c :: !edges;
                                                       edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                                       (baseprefix, c, gen + 1) :: accu
                                                   | None -> accu)
                                                 accu fam_link.MLink.Family.children)
                                             accu family_link
                                         in
                                         children_link
                                       end
                                     else accu
                                 | None -> accu)
                            l families
                        in
                        loop_desc l
                      end
              in
              loop_desc [(conf.command, p, gen)]
            in

            loop l
          end
  in
  nodes := create_node p (Adef.ifam_of_int (-1)) 1 Root conf.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges, List.rev !families)
;;


(* ********************************************************************* *)
(*  [Fonc] print_graph_tree_full : conf -> base ->                       *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
    [Retour] :
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_graph_tree_full conf base =
  let params = get_params conf Mext_read.parse_graph_tree_params in
  let ip = Adef.iper_of_int (Int32.to_int params.Mread.Graph_tree_params.index) in
  let () = Perso.build_sosa_ht conf base in
  let p = poi base ip in
  let max_asc = 12 in
  let nb_asc =
    match params.Mread.Graph_tree_params.nb_asc with
    | Some n -> min max_asc (max (Int32.to_int n) 1)
    | None -> max_asc
  in
  (* cache lien inter arbre *)
  let () = Perso_link.init_cache conf base ip 1 1 1 in
  let (nodes_asc, edges_asc, families_asc) =
    build_graph_asc_full conf base p nb_asc
  in
  (*
  let nodes_asc =
    List.rev_map
      (fun p ->
        let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
        let p = pers_to_piqi_person_tree_full conf base p in
        Mread.Node_full.({
          id = id;
          person = p;
          ifam = None;
        }))
      nodes_asc
  in
  *)
  let max_desc = 12 in
  let nb_desc =
    match params.Mread.Graph_tree_params.nb_desc with
    | Some n -> min max_desc (max (Int32.to_int n) 1)
    | None -> max_desc
  in
  let (nodes_desc, edges_desc, families_desc) =
    build_graph_desc_full conf base p nb_desc
  in
  let nodes_siblings =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        List.fold_right
          (fun ic accu ->
            if ic = ip then accu
            else
              let c = poi base ic in
              (* Pour les liens inter arbres, on rend l'id unique avec *)
              (* le prefix de la base et l'index de la personne.       *)
              let uniq_id = Hashtbl.hash (conf.command, ic) in
              let id = Int64.of_int uniq_id in
              let c = pers_to_piqi_person_tree_full conf base c Siblings 1 1 conf.command in
              let node =
                Mread.Node_full.({
                  id = id;
                  person = c;
                  ifam = None;
                })
              in
              node :: accu)
          (Array.to_list (get_children fam)) []
    | None -> []
  in
  let (nodes_siblings_before, nodes_siblings_after) =
    match get_parents p with
    | Some ifam ->
        let fam = foi base ifam in
        let children = Array.to_list (get_children fam) in
        let rec split_at_person before after l =
          match l with
          | [] -> (List.rev before, after)
          | ic :: l ->
              if ic = ip then
                let after =
                  List.map
                    (fun ic ->
                      let c = poi base ic in
                      (* Pour les liens inter arbres, on rend l'id unique avec *)
                      (* le prefix de la base et l'index de la personne.       *)
                      let uniq_id = Hashtbl.hash (conf.command, ic) in
                      let id = Int64.of_int uniq_id in
                      let c = pers_to_piqi_person_tree_full conf base c Siblings 1 1 conf.command in
                      Mread.Node_full.({
                        id = id;
                        person = c;
                        ifam = None;
                      }))
                    l
                in
                (List.rev before, after)
              else
                let c = poi base ic in
                (* Pour les liens inter arbres, on rend l'id unique avec *)
                (* le prefix de la base et l'index de la personne.       *)
                let uniq_id = Hashtbl.hash (conf.command, ic) in
                let id = Int64.of_int uniq_id in
                let c = pers_to_piqi_person_tree_full conf base c Siblings 1 1 conf.command in
                let node =
                  Mread.Node_full.({
                    id = id;
                    person = c;
                    ifam = None;
                  })
                in
                split_at_person (node :: before) after l
        in
        split_at_person [] [] children
    | None -> ([], [])
  in
  let graph =
    Mread.Graph_tree_full.({
      nodes_asc = nodes_asc;
      edges_asc = edges_asc;
      families_asc = families_asc;
      nodes_desc = nodes_desc;
      edges_desc = edges_desc;
      families_desc = families_desc;
      nodes_siblings = nodes_siblings;
      nodes_siblings_before = nodes_siblings_before;
      nodes_siblings_after = nodes_siblings_after;
    })
  in
  let data = Mext_read.gen_graph_tree_full graph in
  print_result conf data
;;
