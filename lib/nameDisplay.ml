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

let map_first_name_data f conf base person =
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
  if qualifier = "" then esc first_name
  else esc first_name ^^^ " <em>" ^<^ esc qualifier ^>^ "</em>"

let gen_first_name_str ~first_name ~qualifier =
  if qualifier = "" then Adef.safe first_name
  else Adef.safe (first_name ^ " " ^ qualifier)

let first_name_html = map_first_name_data gen_first_name_html
let first_name_str = map_first_name_data gen_first_name_str

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
  gen_fullname first_name_str (fun fn_str surname ->
      let open Def in
      fn_str ^>^ " " ^ surname)

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
