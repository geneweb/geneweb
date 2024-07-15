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

let gen_fullname gen_fn fsurname conf base person =
  let fn = gen_fn conf base person in
  match Gwdb.p_surname base person with
  | "" -> fn
  | surname -> fsurname fn surname

let fullname_html =
  gen_fullname first_name_html (fun fn_html surname ->
      let open Def in
      fn_html ^^^ " " ^<^ esc surname)

let fullname_str_of_person =
  gen_fullname first_name_str (fun fn_str surname ->
      let open Def in
      fn_str ^>^ " " ^ surname)

let first_name_html_of_person conf base person =
  map_person_name_visibility ~on_visible_name:first_name_html conf base person

let fullname_html_of_person conf base person =
  map_person_name_visibility ~on_visible_name:fullname_html conf base person
