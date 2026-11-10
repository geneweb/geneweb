open Def

type t =
  | First_name
  | Surname
  | First_name_alias
  | Surname_alias
  | Public_name
  | Qualifier
  | Alias
  | Occupation
  | Place
  | Source
  | Title
  | Domain
  | Place_last

let all =
  [
    First_name;
    Surname;
    First_name_alias;
    Surname_alias;
    Public_name;
    Qualifier;
    Alias;
    Occupation;
    Place;
    Source;
    Title;
    Domain;
    Place_last;
  ]

let to_string d =
  match d with
  | First_name -> "fn"
  | Surname -> "sn"
  | First_name_alias -> "fna"
  | Surname_alias -> "sna"
  | Public_name -> "pubn"
  | Qualifier -> "qual"
  | Alias -> "alias"
  | Occupation -> "occu"
  | Place -> "place"
  | Source -> "src"
  | Title -> "title"
  | Domain -> "domain"
  | Place_last -> "place_last"

let of_string s =
  List.find_opt
    (fun d ->
      match d with
      | Place_last -> false
      | First_name | Surname | First_name_alias | Surname_alias | Public_name
      | Qualifier | Alias | Occupation | Place | Source | Title | Domain ->
          to_string d = s)
    all

let person_strings d p =
  match d with
  | First_name -> [ p.first_name ]
  | Surname -> [ p.surname ]
  | First_name_alias -> p.first_names_aliases
  | Surname_alias -> p.surnames_aliases
  | Public_name -> [ p.public_name ]
  | Qualifier -> p.qualifiers
  | Alias -> p.aliases
  | Occupation -> [ p.occupation ]
  | Place | Place_last ->
      List.fold_left
        (fun l e -> e.epers_place :: l)
        [ p.birth_place; p.baptism_place; p.death_place; p.burial_place ]
        p.pevents
  | Source ->
      List.fold_left
        (fun l e -> e.epers_src :: l)
        [ p.birth_src; p.baptism_src; p.death_src; p.burial_src; p.psources ]
        p.pevents
  | Title -> List.map (fun t -> t.t_ident) p.titles
  | Domain -> List.map (fun t -> t.t_place) p.titles

let family_strings d f =
  match d with
  | Place | Place_last ->
      List.fold_left
        (fun l e -> e.efam_place :: l)
        [ f.marriage_place ] f.fevents
  | Source ->
      List.fold_left
        (fun l e -> e.efam_src :: l)
        [ f.marriage_src; f.fsources ]
        f.fevents
  | First_name | Surname | First_name_alias | Surname_alias | Public_name
  | Qualifier | Alias | Occupation | Title | Domain ->
      []

let records_dict d =
  match d with
  | Place_last -> Place
  | First_name | Surname | First_name_alias | Surname_alias | Public_name
  | Qualifier | Alias | Occupation | Place | Source | Title | Domain ->
      d

let has_family_fields d =
  match d with
  | Place | Place_last | Source -> true
  | First_name | Surname | First_name_alias | Surname_alias | Public_name
  | Qualifier | Alias | Occupation | Title | Domain ->
      false

let suburb s =
  let len = String.length s in
  if len = 0 || s.[0] <> '[' then None
  else
    match String.index_opt s ']' with
    | None -> None
    | Some i ->
        let rec loop dash j =
          if j = len then None
          else
            match s.[j] with
            | ' ' -> loop dash (j + 1)
            | '-' when not dash -> loop true (j + 1)
            | '\xE2'
              when (not dash)
                   && j + 2 < len
                   && s.[j + 1] = '\x80'
                   && (s.[j + 2] = '\x93' || s.[j + 2] = '\x94') ->
                loop true (j + 3)
            | _ -> if dash then Some (i, j) else None
        in
        loop false (i + 1)

let without_suburb s =
  match suburb s with
  | Some (_, j) -> String.sub s j (String.length s - j)
  | None -> s

let paren_form t =
  let len = String.length t in
  if len > 2 && t.[len - 1] = ')' then
    match String.rindex_opt t '(' with
    | Some i when i > 0 ->
        let host = String.trim (String.sub t 0 i) in
        let parent = String.trim (String.sub t (i + 1) (len - i - 2)) in
        if host = "" || parent = "" then None else Some (host, parent)
    | Some _ | None -> None
  else None

let split_paren_term t =
  match paren_form t with
  | Some (host, parent) -> [ host; parent ]
  | None -> [ t ]

let last_terms s =
  let terms =
    String.split_on_char ',' (without_suburb s)
    |> List.filter_map (fun t ->
        let t = String.trim t in
        if t = "" then None else Some t)
  in
  let rec loop acc l =
    match l with
    | [] -> acc
    | t :: l -> loop (List.rev_append (split_paren_term t) acc) l
  in
  loop [] terms

let last_terms_sep = ", "
let last_key s = String.concat last_terms_sep (last_terms s)

let key particles d s =
  match d with
  | Surname | Domain ->
      let p = Mutil.get_particle particles s in
      let n = String.length p in
      String.concat "\000" [ String.sub s n (String.length s - n); p ]
  | Place -> (
      match suburb s with
      | Some (_, j) ->
          String.concat "\000" [ String.sub s j (String.length s - j); s ]
      | None -> s ^ "\000")
  | Place_last -> String.concat "\000" [ last_key s; s ]
  | First_name | First_name_alias | Surname_alias | Public_name | Qualifier
  | Alias | Occupation | Source | Title ->
      s
