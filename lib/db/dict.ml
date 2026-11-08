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

let of_string s = List.find_opt (fun d -> to_string d = s) all

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
  | Place ->
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
  | Place ->
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

let has_family_fields d =
  match d with
  | Place | Source -> true
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
  | First_name | First_name_alias | Surname_alias | Public_name | Qualifier
  | Alias | Occupation | Source | Title ->
      s
