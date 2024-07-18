let esc x = (Util.escape_html x :> Adef.safe_string)
let restricted_txt = Adef.safe "....."
let x_x_txt = Adef.safe "x x"
let hidden_name_txt = Adef.safe "x"

module NameVisibilityUtil : sig
  type t = Gwdb.person
  type name_visibility = HiddenName | RestrictedName | VisibleName of t

  val name_visibility_of_person :
    conf:Config.config ->
    base:Gwdb.base ->
    person:Gwdb.person ->
    name_visibility
end = struct
  type t = Gwdb.person
  type name_visibility = HiddenName | RestrictedName | VisibleName of t

  let is_hidden conf base person =
    Util.is_hide_names conf person && not (Util.authorized_age conf base person)

  let name_visibility_of_person ~conf ~base ~person =
    if Util.is_empty_person person then RestrictedName
    else if is_hidden conf base person then HiddenName
    else VisibleName person
end

let is_hidden conf base person =
  NameVisibilityUtil.(
    name_visibility_of_person ~conf ~base ~person = HiddenName)

let is_restricted conf base person =
  NameVisibilityUtil.(
    name_visibility_of_person ~conf ~base ~person = RestrictedName)

let map_person_name_visibility' ~on_hidden_name ~on_restricted_name
    ~on_visible_name ~conf ~base ~person =
  match NameVisibilityUtil.name_visibility_of_person ~conf ~base ~person with
  | NameVisibilityUtil.HiddenName -> on_hidden_name conf base person
  | RestrictedName -> on_restricted_name conf base person
  | VisibleName _name_data -> on_visible_name conf base person

let map_person_name_visibility ?(on_hidden_name = fun _ _ _ -> x_x_txt)
    ?(on_restricted_name = fun _ _ _ -> restricted_txt) ~on_visible_name conf
    base person =
  map_person_name_visibility' ~on_hidden_name ~on_restricted_name
    ~on_visible_name ~conf ~base ~person

let map_first_name_data :
    type a.
    (first_name:string -> qualifier:string -> a) ->
    Config.config ->
    Gwdb.base ->
    Gwdb.person ->
    a =
 fun f conf base person ->
  let first_name =
    match Gwdb.sou base (Gwdb.get_public_name person) with
    | "" -> Gwdb.p_first_name base person
    | public_name -> public_name
  in
  match Gwdb.get_qualifiers person with
  | [] -> f ~first_name ~qualifier:""
  | qualifier :: _ ->
      let qualifier = Gwdb.sou base qualifier in
      f ~first_name ~qualifier

let gen_first_name_html ~first_name ~qualifier =
  let open Def in
  if qualifier = "" then Adef.as_string (esc first_name)
  else Adef.as_string (esc first_name ^^^ " <em>" ^<^ esc qualifier ^>^ "</em>")

let gen_first_name_str ~first_name ~qualifier =
  if qualifier = "" then first_name else first_name ^ " " ^ qualifier

let first_name_html conf base person =
  Adef.safe (map_first_name_data gen_first_name_html conf base person)

let first_name_str_of_person conf base person =
  map_first_name_data gen_first_name_str conf base person

let map_fullname_data f conf base person =
  let surname = Gwdb.p_surname base person in
  map_first_name_data (f ~surname) conf base person

let gen_fullname ?(p_surname = Gwdb.p_surname) gen_fn fsurname conf base person
    =
  let fn = gen_fn conf base person in
  match p_surname base person with "" -> fn | surname -> fsurname fn surname

let fullname_html ~p_surname =
  gen_fullname ~p_surname first_name_html (fun fn_html surname ->
      let open Def in
      fn_html ^^^ " " ^<^ esc surname)

let fullname_str_of_person =
  gen_fullname first_name_str_of_person (fun fn_str surname ->
      fn_str ^ " " ^ surname)

let first_name_html_of_person conf base person =
  map_person_name_visibility ~on_visible_name:first_name_html conf base person

let fullname_html_of_person ?(p_surname = Gwdb.p_surname) conf base person =
  map_person_name_visibility ~on_visible_name:(fullname_html ~p_surname) conf
    base person

let title_html_of_person conf base p t : Adef.safe_string =
  if List.assoc_opt "print_advanced_title" conf.Config.base_env = Some "yes"
  then
    let estate = Gwdb.sou base t.Def.t_place in
    let surname = Gwdb.p_surname base p in
    (* Si le nom de l'individu est le même que son domaine, on renvoie : *)
    (*   - le nom du titre                                               *)
    (*   - le nom du titre et le premier sobriquet                       *)
    (*   - le nom de la personne (donné par son nom de domaine) en       *)
    (*     fonction du nom public et sobriquet                           *)
    if Name.strip_lower estate = Name.strip_lower surname then
      match (t.Def.t_name, Gwdb.get_qualifiers p) with
      | Def.Tname n, [] -> (esc (Gwdb.sou base n) :> Adef.safe_string)
      | Def.Tname n, nn :: _ ->
          let open Def in
          (esc (Gwdb.sou base n) :> Adef.safe_string)
          ^^^ " <em>"
          ^<^ (esc (Gwdb.sou base nn) :> Adef.safe_string)
          ^>^ "</em>"
      | _ -> first_name_html_of_person conf base p
    else
      let elen = String.length estate in
      let slen = String.length surname in
      if elen < slen && String.sub surname (slen - elen) elen = estate then
        match (t.Def.t_name, Gwdb.get_qualifiers p) with
        | Def.Tname n, [] -> esc (Gwdb.sou base n)
        | Def.Tname n, nn :: _ ->
            let open Def in
            esc (Gwdb.sou base n)
            ^^^ " <em>"
            ^<^ esc (Gwdb.sou base nn)
            ^>^ "</em>"
        | _ ->
            fullname_html_of_person
              ~p_surname:(fun _ _ ->
                String.trim (String.sub surname 0 (slen - elen)))
              conf base p
      else
        match t.Def.t_name with
        | Def.Tname s -> (
            let s = esc (Gwdb.sou base s) in
            match Gwdb.get_qualifiers p with
            | [] -> s
            | nn :: _ ->
                let open Def in
                s ^^^ " <em>" ^<^ esc (Gwdb.sou base nn) ^>^ "</em>")
        | _ -> fullname_html_of_person conf base p
  else fullname_html_of_person conf base p

let fullname_html_of_person = fullname_html_of_person ~p_surname:Gwdb.p_surname

let gen_person_title_text reference conf base p =
  if Util.authorized_age conf base p then
    match Util.main_title conf base p with
    | Some t ->
        let open Def in
        reference conf base p (title_html_of_person conf base p t)
        ^^^ ", " ^<^ Util.one_title_text base t
    | None -> reference conf base p (fullname_html_of_person conf base p)
  else reference conf base p (fullname_html_of_person conf base p)

let reference_flags with_id conf base p (s : Adef.safe_string) =
  let iper = Gwdb.get_iper p in
  if Util.is_empty_person p then s
  else
    let open Def in
    "<a href=\""
    ^<^ (Util.commd conf ^^^ Util.acces conf base p :> Adef.safe_string)
    ^^^ (if with_id then "\" id=\"i" else "")
    ^<^ (if with_id then Gwdb.string_of_iper iper else "")
    ^<^ "\">" ^<^ s ^>^ "</a>"

let reference = reference_flags true
let reference_noid = reference_flags false
let no_reference _conf _base _p s = s
let referenced_person_title_text = gen_person_title_text reference
let person_title_text = gen_person_title_text no_reference

let referenced_person_text conf base p =
  reference conf base p (fullname_html_of_person conf base p)

let referenced_person_text_without_surname conf base p =
  reference conf base p (first_name_html_of_person conf base p)

let person_text_without_title conf base p =
  match Util.main_title conf base p with
  | Some t -> (
      if Gwdb.eq_istr t.Def.t_place (Gwdb.get_surname p) then
        first_name_html_of_person conf base p
      else
        match (t.Def.t_name, Gwdb.get_qualifiers p) with
        | Def.Tname s, nn :: _ ->
            let open Def in
            esc (Gwdb.sou base s)
            ^^^ " <em>"
            ^<^ esc (Gwdb.sou base nn)
            ^>^ "</em>"
        | Def.Tname s, _ -> esc (Gwdb.sou base s)
        | _ -> fullname_html_of_person conf base p)
  | None -> fullname_html_of_person conf base p

let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (Gwdb.eq_istr (Gwdb.get_surname p) (Gwdb.get_surname fath)) then
      fullname_html_of_person conf base fath
    else first_name_html_of_person conf base fath
  in
  let a = Util.pget conf base (Gwdb.get_iper p) in
  let ifam =
    match Gwdb.get_parents a with
    | Some ifam ->
        let cpl = Gwdb.foi base ifam in
        let fath =
          let fath = Util.pget conf base (Gwdb.get_father cpl) in
          if Gwdb.p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = Util.pget conf base (Gwdb.get_mother cpl) in
          if Gwdb.p_first_name base moth = "?" then None else Some moth
        in
        Some (fath, moth)
    | None -> None
  in
  match ifam with
  | Some (None, None) | None -> Adef.safe ""
  | Some (fath, moth) ->
      let s =
        match (fath, moth) with
        | Some fath, None -> print_father fath
        | None, Some moth -> fullname_html_of_person conf base moth
        | Some fath, Some moth ->
            let open Def in
            print_father fath ^^^ " "
            ^<^ Util.transl_nth conf "and" 0
            ^<^ " "
            ^<^ fullname_html_of_person conf base moth
        | _ -> Adef.safe ""
      in
      let is = Util.index_of_sex (Gwdb.get_sex p) in
      let s = (s :> string) in
      Util.transl_a_of_gr_eq_gen_lev conf
        (Util.transl_nth conf "son/daughter/child" is)
        s s
      |> Util.translate_eval |> Adef.safe

let relation_date conf base fam : Adef.safe_string =
  let is_visible family =
    let is_visible person =
      Util.authorized_age conf base (Util.pget conf base person)
    in
    is_visible (Gwdb.get_father family) && is_visible (Gwdb.get_mother family)
  in
  Adef.safe
  @@
  if not @@ is_visible fam then ""
  else
    match Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) with
    | None -> ""
    | Some dmy ->
        " " ^ Util.transl conf "in (year)" ^ " " ^ string_of_int dmy.year

let husband_wife conf base p all =
  let relation =
    let rec loop i =
      if i < Array.length (Gwdb.get_family p) then
        let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
        let conjoint = Gutil.spouse (Gwdb.get_iper p) fam in
        let conjoint = Util.pget conf base conjoint in
        if not @@ Util.is_empty_name conjoint then
          Printf.sprintf
            (Util.relation_txt conf (Gwdb.get_sex p) fam)
            (fun () -> "")
          |> Util.translate_eval |> Adef.safe
        else loop (i + 1)
      else Adef.safe ""
    in
    loop 0
  in
  let res =
    let rec loop i res =
      if i < Array.length (Gwdb.get_family p) then
        let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
        let conjoint = Gutil.spouse (Gwdb.get_iper p) fam in
        let conjoint = Util.pget conf base conjoint in
        if not @@ Util.is_empty_name conjoint then
          let res =
            let open Def in
            res
            ^>^ Util.translate_eval
                  (" "
                   ^<^ fullname_html_of_person conf base conjoint
                   ^^^ relation_date conf base fam
                    :> string)
            ^ ","
          in
          if all then loop (i + 1) res else res
        else loop (i + 1) res
      else res
    in
    loop 0 relation
  in
  let res = (res :> string) in
  let res =
    if String.length res > 1 then String.sub res 0 (String.length res - 1)
    else res
  in
  Adef.safe res

let first_child conf base p =
  let is = Util.index_of_sex (Gwdb.get_sex p) in
  let rec loop i =
    if i < Array.length (Gwdb.get_family p) then
      let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
      let ct = Gwdb.get_children fam in
      if Array.length ct > 0 then
        let enfant = Util.pget conf base ct.(0) in
        let child =
          if
            Util.is_hide_names conf enfant
            && not (Util.authorized_age conf base enfant)
          then Adef.safe "xx"
          else if
            not (Gwdb.eq_istr (Gwdb.get_surname p) (Gwdb.get_surname enfant))
          then fullname_html_of_person conf base enfant
          else first_name_html_of_person conf base enfant
        in
        let child = (child :> string) in
        Util.transl_a_of_b conf
          (Util.transl_nth conf "father/mother" is)
          child child
        |> Util.translate_eval |> Adef.safe
      else loop (i + 1)
    else Adef.safe ""
  in
  loop 0

let specify_homonymous conf base p specify_public_name =
  match (Gwdb.get_public_name p, Gwdb.get_qualifiers p) with
  | n, nn :: _ when Gwdb.sou base n <> "" && specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Gwdb.sou base n);
      Output.print_sstring conf " <em>";
      Output.print_string conf (esc @@ Gwdb.sou base nn);
      Output.print_sstring conf "</em>"
  | _, nn :: _ when specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Gwdb.p_first_name base p);
      Output.print_sstring conf " <em>";
      Output.print_string conf (esc @@ Gwdb.sou base nn);
      Output.print_sstring conf "</em>"
  | n, [] when Gwdb.sou base n <> "" && specify_public_name ->
      Output.print_sstring conf " ";
      Output.print_string conf (esc @@ Gwdb.sou base n)
  | _, _ ->
      (* Le nom public et le qualificatif ne permettent pas de distinguer *)
      (* la personne, donc on affiche les informations sur les parents,   *)
      (* le mariage et/ou le premier enfant.                              *)
      let cop = child_of_parent conf base p in
      if (cop :> string) <> "" then (
        Output.print_sstring conf ", ";
        Output.print_string conf cop);
      let hw = husband_wife conf base p true in
      if (hw :> string) = "" then (
        let fc = first_child conf base p in
        if (fc :> string) <> "" then (
          Output.print_sstring conf ", ";
          Output.print_string conf fc))
      else (
        Output.print_sstring conf ", ";
        Output.print_string conf hw)
