(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let get_number var key env = p_getint env (var ^ "_" ^ key)

let reconstitute_date_dmy conf var =
  match get_number var "yyyy" conf.env with
  | Some y -> (
      match get_number var "mm" conf.env with
      | Some m -> (
          match get_number var "dd" conf.env with
          | Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                Some
                  Date.{ day = d; month = m; year = y; prec = Sure; delta = 0 }
              else None
          | None ->
              if m >= 1 && m <= 12 then
                Some { day = 0; month = m; year = y; prec = Sure; delta = 0 }
              else None)
      | None -> Some { day = 0; month = 0; year = y; prec = Sure; delta = 0 })
  | None -> None

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i

let string_incl x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then
          if j = String.length y then true
          else String.unsafe_get y j = ' ' || String.unsafe_get y (j - 1) = ' '
        else if
          j < String.length y && String.unsafe_get x i = String.unsafe_get y j
        then loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0

let abbrev_lower x = Name.abbrev (Name.lower x)
let sex_of_string = function "M" -> Def.Male | "F" -> Female | _ -> Neuter

module Fields : sig
  type search = And | Or
  type name = string

  val get_event_field_name :
    (string -> string) -> string -> string -> search -> name

  val bapt_date : gets:(string -> string) -> search_type:search -> name
  val birth_date : gets:(string -> string) -> search_type:search -> name
  val death_date : gets:(string -> string) -> search_type:search -> name
  val burial_date : gets:(string -> string) -> search_type:search -> name
  val marriage_date : gets:(string -> string) -> search_type:search -> name
  val other_events_date : gets:(string -> string) -> search_type:search -> name
  val bapt_place : gets:(string -> string) -> search_type:search -> name
  val birth_place : gets:(string -> string) -> search_type:search -> name
  val death_place : gets:(string -> string) -> search_type:search -> name
  val burial_place : gets:(string -> string) -> search_type:search -> name
  val marriage_place : gets:(string -> string) -> search_type:search -> name
  val other_events_place : gets:(string -> string) -> search_type:search -> name
end = struct
  type search = And | Or
  type name = string

  (* Get the field name of an event criteria depending of the search type. *)
  let get_event_field_name gets event_criteria event_name search_type =
    match search_type with
    | And -> event_name ^ "_" ^ event_criteria
    | Or -> if "on" = gets ("event_" ^ event_name) then event_criteria else ""

  let bapt_date ~gets ~search_type =
    get_event_field_name gets "date" "bapt" search_type

  let birth_date ~gets ~search_type =
    get_event_field_name gets "date" "birth" search_type

  let death_date ~gets ~search_type =
    get_event_field_name gets "date" "death" search_type

  let burial_date ~gets ~search_type =
    get_event_field_name gets "date" "burial" search_type

  let marriage_date ~gets ~search_type =
    get_event_field_name gets "date" "marriage" search_type

  let other_events_date ~gets ~search_type =
    get_event_field_name gets "date" "other_events" search_type

  let bapt_place ~gets ~search_type =
    get_event_field_name gets "place" "bapt" search_type

  let birth_place ~gets ~search_type =
    get_event_field_name gets "place" "birth" search_type

  let death_place ~gets ~search_type =
    get_event_field_name gets "place" "death" search_type

  let burial_place ~gets ~search_type =
    get_event_field_name gets "place" "burial" search_type

  let marriage_place ~gets ~search_type =
    get_event_field_name gets "place" "marriage" search_type

  let other_events_place ~gets ~search_type =
    get_event_field_name gets "place" "other_events" search_type
end

module AdvancedSearchMatch : sig
  val match_name :
    search_list:string list list -> exact:bool -> string list -> bool

  val match_civil_status :
    base:Gwdb.base ->
    p:Gwdb.person ->
    sex:Def.sex ->
    married:string ->
    occupation:string ->
    first_name_list:string list list ->
    surname_list:string list list ->
    skip_fname:bool ->
    skip_sname:bool ->
    exact_first_name:bool ->
    exact_surname:bool ->
    bool

  val match_marriage :
    exact_place:bool ->
    conf:Config.config ->
    base:Gwdb.base ->
    p:Gwdb.person ->
    places:string list ->
    default:bool ->
    dates:Date.dmy option * Date.dmy option ->
    bool

  module type Match = sig
    val match_baptism :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_birth :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_burial :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_death :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_other_events :
      conf:Config.config ->
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool
  end

  module And : Match
  module Or : Match
end = struct
  (* Check if the date matches with the person event. *)
  let match_date ~p ~df ~default ~dates =
    let d1, d2 = dates in
    match (d1, d2) with
    | Some d1, Some d2 -> (
        match df p with
        | Some d -> Date.compare_dmy d d1 >= 0 && Date.compare_dmy d d2 <= 0
        | None -> false)
    | Some d1, None -> (
        match df p with Some d -> Date.compare_dmy d d1 >= 0 | None -> false)
    | None, Some d2 -> (
        match df p with Some d -> Date.compare_dmy d d2 <= 0 | None -> false)
    | None, None -> default

  let do_compare ~p ~places ~get ~cmp =
    let s = abbrev_lower @@ get p in
    List.exists (fun s' -> cmp (abbrev_lower s') s) places

  let apply_to_field_places_raw ~cmp ~p ~places ~get ~default =
    if places = [] then default else do_compare ~p ~places ~get ~cmp

  let apply_to_field_places ~get ~cmp ~base =
    apply_to_field_places_raw ~get:(fun p -> sou base @@ get p) ~cmp

  let match_sex ~p ~sex = if sex = Def.Neuter then true else get_sex p = sex

  let married_cmp p = function
    | "Y" -> get_family p <> [||]
    | "N" -> get_family p = [||]
    | _ -> true

  let match_married ~p ~married =
    if married = "" then true else married_cmp p married

  let exact_place_wrapper f ~exact_place =
    let cmp = if exact_place then ( = ) else string_incl in
    f ~cmp

  let match_baptism_place =
    exact_place_wrapper @@ apply_to_field_places ~get:get_baptism_place

  let match_birth_place =
    exact_place_wrapper @@ apply_to_field_places ~get:get_birth_place

  let match_death_place =
    exact_place_wrapper @@ apply_to_field_places ~get:get_death_place

  let match_burial_place =
    exact_place_wrapper @@ apply_to_field_places ~get:get_burial_place

  let match_other_event_place =
    exact_place_wrapper @@ apply_to_field_places ~get:Event.get_place

  let match_other_events_place ~exact_place ~conf ~base ~p ~places ~default =
    if places = [] then default
    else
      List.exists
        (fun e ->
          match_other_event_place ~exact_place ~base ~places ~default:false ~p:e)
        (Event.other_events conf base p)

  let match_marriage ~cmp ~conf ~base ~p ~places ~default ~dates =
    let d1, d2 = dates in
    let test_date_place df =
      Array.exists
        (fun ifam ->
          let fam = foi base ifam in
          let sp = poi base @@ Gutil.spouse (get_iper p) fam in
          if authorized_age conf base sp then
            df fam
            && (places = []
               || do_compare ~p:fam ~places
                    ~get:(fun f -> sou base @@ get_marriage_place f)
                    ~cmp)
          else false)
        (get_family p)
    in
    match (d1, d2) with
    | Some d1, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d ->
                if Date.compare_dmy d d1 < 0 then false
                else if Date.compare_dmy d2 d < 0 then false
                else true
            | None -> false)
    | Some d1, None ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d -> if Date.compare_dmy d d1 < 0 then false else true
            | None -> false)
    | None, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d -> if Date.compare_dmy d d2 > 0 then false else true
            | None -> false)
    | None, None ->
        if places = [] then default else test_date_place (fun _ -> true)

  let match_marriage = exact_place_wrapper match_marriage

  let match_occupation ~base ~p ~occupation =
    if occupation = "" then true
    else
      string_incl (abbrev_lower occupation)
        (abbrev_lower @@ sou base @@ get_occupation p)

  let match_baptism_date =
    match_date ~df:(fun p -> Date.cdate_to_dmy_opt (get_baptism p))

  let match_birth_date =
    match_date ~df:(fun p -> Date.cdate_to_dmy_opt (get_birth p))

  let match_burial_date =
    let get_burial p =
      (* TODO Date.cdate_of_burial *)
      match get_burial p with
      | Buried cod | Cremated cod -> Date.cdate_to_dmy_opt cod
      | UnknownBurial -> None
    in
    match_date ~df:get_burial

  let match_death_date =
    match_date ~df:(fun p -> Date.dmy_of_death (get_death p))

  let match_other_events_date ~conf ~base ~p ~default ~dates =
    if dates = (None, None) then default
    else
      p
      |> Event.other_events conf base
      |> List.map (fun e (* wrap value in unit -> dmy to be lazy ?*) () ->
             Date.cdate_to_dmy_opt @@ Event.get_date e)
      |> List.exists (fun event_date_f ->
             match_date ~p ~default:false ~dates ~df:(fun _ -> event_date_f ()))

  let match_name ~search_list ~exact : string list -> bool =
    let matching : string list -> string list -> bool =
      if exact then Ext_list.elements_cmp else Ext_list.is_subset
    in
    fun x -> List.exists (fun s -> matching s x) search_list

  let wrap_match_name ~base ~search_list ~exact ~get ~split =
    if search_list = [] then fun _ -> true
    else
      let eq = match_name ~search_list ~exact in
      fun p -> eq (List.map Name.lower @@ split @@ sou base @@ get p)

  let match_first_name ~base ~first_name_list ~exact =
    wrap_match_name ~base ~search_list:first_name_list ~exact
      ~get:get_first_name ~split:Name.split_fname

  let match_surname ~base ~surname_list ~exact =
    wrap_match_name ~base ~search_list:surname_list ~exact ~get:get_surname
      ~split:Name.split_sname

  let match_alias ~base ~alias_list ~exact ~kind p =
    let gets =
      let get =
        match kind with
        | `First_name -> Gwdb.get_first_names_aliases
        | `Surname -> Gwdb.get_surnames_aliases
      in
      List.map (fun alias _ -> alias) (get p)
    in
    let split =
      match kind with
      | `Surname -> Name.split_sname
      | `First_name -> Name.split_fname
    in
    List.exists
      (fun get ->
        wrap_match_name ~base ~search_list:alias_list ~get ~exact ~split p)
      gets

  (* We use [first_name_list] as the list of aliases to search for, so
     searching for a first name will also look at first name aliases. *)
  let match_first_name_alias ~base ~first_name_list ~exact p =
    match_alias ~base ~alias_list:first_name_list ~exact ~kind:`First_name p

  let match_surname_alias ~base ~surname_list ~exact p =
    match_alias ~base ~alias_list:surname_list ~exact ~kind:`Surname p

  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status ~base ~p ~sex ~married ~occupation ~first_name_list
      ~surname_list ~skip_fname ~skip_sname ~exact_first_name ~exact_surname =
    match_sex ~p ~sex
    && (skip_fname
       || match_first_name ~base ~first_name_list ~exact:exact_first_name p
       || match_first_name_alias ~base ~first_name_list ~exact:exact_first_name
            p)
    && (skip_sname
       || match_surname ~base ~surname_list ~exact:exact_surname p
       || match_surname_alias ~base ~surname_list ~exact:exact_surname p)
    && match_married ~p ~married
    && match_occupation ~base ~p ~occupation

  module type Match = sig
    val match_baptism :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_birth :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_burial :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_death :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool

    val match_other_events :
      conf:Config.config ->
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool
  end

  module And = struct
    let match_and date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : string list) ~(exact_place : bool) =
      date_f ~p ~default:true ~dates
      && place_f ~exact_place ~base ~p ~places ~default:true

    let match_baptism = match_and match_baptism_date match_baptism_place
    let match_birth = match_and match_birth_date match_birth_place
    let match_burial = match_and match_burial_date match_burial_place
    let match_death = match_and match_death_date match_death_place

    let match_other_events ~conf ~base =
      match_and ~base
        (match_other_events_date ~conf ~base)
        (match_other_events_place ~conf)
  end

  module Or = struct
    let match_or date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : string list) ~(exact_place : bool) =
      date_f ~p ~default:false ~dates
      || place_f ~exact_place ~base ~p ~places ~default:false

    let match_baptism = match_or match_baptism_date match_baptism_place
    let match_birth = match_or match_birth_date match_birth_place
    let match_burial = match_or match_burial_date match_burial_place
    let match_death = match_or match_death_date match_death_place

    let match_other_events ~conf ~base =
      match_or ~base
        (match_other_events_date ~conf ~base)
        (match_other_events_place ~conf)
  end
end

(* Search type can be AND or OR. *)
let get_search_type gets =
  match gets "search_type" with "OR" -> Fields.Or | _ -> Fields.And

(*
  Search for other persons in the base matching with the provided infos.

  On search semantic:

   Search can be set to be exact on the first name and/or the surname,
   if no first name or surname is provided then the search ignores the
   parameter in both the exact and the loose case.

   - When search is loose it is only necessary for each name atom (name atoms
   for "Jean-Pierre" are: [Jean] [Pierre]) to be found at least once in another
   person's name atoms in the base.

   - When search is exact, it is necessary for each atom to be found exactly the
   number of times it occurs in the given name but order is not considered for
   a person from the base to match. (ie. "Pierre-Jean de Bourbon de Vallois" matches
   with "Jean Pierre de Vallois de Bourbon" but not with "Jean de Bourbon")
*)
let advanced_search conf base max_answers =
  let open AdvancedSearchMatch in
  let hs = Hashtbl.create 73 in
  let hss = Hashtbl.create 73 in
  let hd = Hashtbl.create 73 in
  let getd x =
    try Hashtbl.find hd x
    with Not_found ->
      let v =
        ( reconstitute_date_dmy conf (x ^ "1"),
          reconstitute_date_dmy conf (x ^ "2") )
      in
      Hashtbl.add hd x v;
      v
  in
  let gets x =
    try Hashtbl.find hs x
    with Not_found ->
      let v = match p_getenv conf.env x with Some v -> v | None -> "" in
      Hashtbl.add hs x v;
      v
  in
  let getss x =
    let y = gets x in
    if y <> "" then [ y ]
    else
      match Hashtbl.find_opt hss x with
      | Some v -> v
      | None ->
          let rec loop acc i =
            let k = x ^ "_" ^ string_of_int i in
            match p_getenv conf.env k with
            | Some v -> loop (if v <> "" then v :: acc else acc) (i + 1)
            | None -> acc
          in
          let v = loop [] 1 in
          Hashtbl.add hss x v;
          v
  in
  let fn_list =
    List.map
      (fun s -> List.map Name.lower @@ Name.split_fname s)
      (getss "first_name")
  in
  let sn_list =
    List.map
      (fun s -> List.map Name.lower @@ Name.split_sname s)
      (getss "surname")
  in
  let search_type = get_search_type gets in

  let exact_place = "on" = gets "exact_place" in

  let match_person ?(skip_fname = false) ?(skip_sname = false)
      ((list, len) as acc) p search_type =
    let auth = authorized_age conf base p in
    let civil_match =
      lazy
        (match_civil_status ~base ~p
           ~sex:(gets "sex" |> sex_of_string)
           ~married:(gets "married") ~occupation:(gets "occu") ~skip_fname
           ~skip_sname ~first_name_list:fn_list ~surname_list:sn_list
           ~exact_first_name:(gets "exact_first_name" = "on")
           ~exact_surname:(gets "exact_surname" = "on"))
    in
    let pmatch =
      match search_type with
      | _ when not auth -> false
      | Fields.Or ->
          let match_f ~date_field ~place_field or_f and_f =
            or_f ~base ~p
              ~dates:(getd @@ date_field ~gets ~search_type)
              ~places:(getss @@ place_field ~gets ~search_type)
              ~exact_place
            && and_f ~base ~p
                 ~dates:(getd @@ date_field ~gets ~search_type)
                 ~places:(getss @@ place_field ~gets ~search_type)
                 ~exact_place
          in
          Lazy.force civil_match
          && (getss "place" = []
              && gets "date2_yyyy" = ""
              && gets "date1_yyyy" = ""
             || match_f ~date_field:Fields.bapt_date
                  ~place_field:Fields.bapt_place Or.match_baptism
                  And.match_baptism
             || match_f ~date_field:Fields.birth_date
                  ~place_field:Fields.birth_place Or.match_birth And.match_birth
             || match_f ~date_field:Fields.burial_date
                  ~place_field:Fields.burial_place Or.match_burial
                  And.match_burial
             || match_f ~date_field:Fields.death_date
                  ~place_field:Fields.death_place Or.match_death And.match_death
             || match_marriage ~conf ~base ~p ~exact_place ~default:false
                  ~places:(getss @@ Fields.marriage_place ~gets ~search_type)
                  ~dates:(getd @@ Fields.marriage_date ~gets ~search_type)
             || match_f ~date_field:Fields.other_events_date
                  ~place_field:Fields.other_events_place
                  (Or.match_other_events ~conf)
                  (And.match_other_events ~conf))
      | _ ->
          Lazy.force civil_match
          && And.match_baptism ~base ~p ~exact_place
               ~dates:(getd @@ Fields.bapt_date ~gets ~search_type)
               ~places:(getss @@ Fields.bapt_place ~gets ~search_type)
          && And.match_birth ~base ~p ~exact_place
               ~dates:(getd @@ Fields.birth_date ~gets ~search_type)
               ~places:(getss @@ Fields.birth_place ~gets ~search_type)
          && And.match_burial ~base ~p ~exact_place
               ~dates:(getd @@ Fields.burial_date ~gets ~search_type)
               ~places:(getss @@ Fields.burial_place ~gets ~search_type)
          && And.match_death ~base ~p ~exact_place
               ~dates:(getd @@ Fields.death_date ~gets ~search_type)
               ~places:(getss @@ Fields.death_place ~gets ~search_type)
          && match_marriage ~conf ~base ~p ~exact_place ~default:true
               ~places:(getss @@ Fields.marriage_place ~gets ~search_type)
               ~dates:(getd @@ Fields.marriage_date ~gets ~search_type)
          && And.match_other_events ~conf ~base ~p ~exact_place
               ~dates:(getd @@ Fields.other_events_date ~gets ~search_type)
               ~places:(getss @@ Fields.other_events_place ~gets ~search_type)
    in
    if pmatch then (p :: list, len + 1) else acc
  in
  let list, len =
    if "on" = gets "sosa_filter" then
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
          let rec loop p (set, acc) =
            if not (IperSet.mem (get_iper p) set) then
              let set = IperSet.add (get_iper p) set in
              let acc = match_person acc p search_type in
              match get_parents p with
              | Some ifam ->
                  let fam = foi base ifam in
                  let set, acc =
                    loop (pget conf base @@ get_mother fam) (set, acc)
                  in
                  loop (pget conf base @@ get_father fam) (set, acc)
              | None -> (set, acc)
            else (set, acc)
          in
          loop (pget conf base @@ get_iper sosa_ref) (IperSet.empty, ([], 0))
          |> snd
      | None -> ([], 0)
    else if fn_list <> [] || sn_list <> [] then
      let list_aux strings_of persons_of split n_list exact =
        List.map
          (List.map (fun x ->
               let eq = match_name ~search_list:n_list ~exact in
               let istrs = strings_of base x in
               List.fold_left
                 (fun acc istr ->
                   let str = Mutil.nominative (sou base istr) in
                   if eq (List.map Name.lower @@ split str) then istr :: acc
                   else acc)
                 [] istrs))
          n_list
        |> List.flatten |> List.flatten |> List.sort_uniq compare
        |> List.map (spi_find @@ persons_of base)
        |> List.flatten |> List.sort_uniq compare
      in
      let skip_fname, skip_sname, list =
        if sn_list <> [] then
          ( false,
            true,
            list_aux Gwdb.base_strings_of_surname Gwdb.persons_of_surname
              Name.split_sname sn_list
              (gets "exact_surname" = "on") )
        else
          ( true,
            false,
            list_aux Gwdb.base_strings_of_first_name Gwdb.persons_of_first_name
              Name.split_fname fn_list
              (gets "exact_first_name" = "on") )
      in
      let rec loop ((_, len) as acc) = function
        | [] -> acc
        | _ when len > max_answers -> acc
        | ip :: l ->
            loop
              (match_person ~skip_fname ~skip_sname acc (pget conf base ip)
                 search_type)
              l
      in
      loop ([], 0) list
    else
      Gwdb.Collection.fold_until
        (fun (_, len) -> len <= max_answers)
        (fun acc i -> match_person acc (pget conf base i) search_type)
        ([], 0) (Gwdb.ipers base)
  in
  (List.rev list, len)

(*
  Returns a description string for the current advanced search results in the correct language.
  e.g. "Search all Pierre, born in Paris, died in Paris"
*)
let searching_fields conf base =
  let test_date x =
    reconstitute_date_dmy conf (x ^ "1") <> None
    || reconstitute_date_dmy conf (x ^ "2") <> None
  in
  let gets x =
    match p_getenv conf.env x with
    | Some v when v <> "" -> v
    | _ ->
        let rec loop acc i =
          let k = x ^ "_" ^ string_of_int i in
          match p_getenv conf.env k with
          | Some v ->
              loop
                (if acc = "" then v
                else if v = "" then acc
                else acc ^ " / " ^ v)
                (i + 1)
          | None -> acc
        in
        loop "" 1
  in
  let test_string x = gets x <> "" in
  let getd x =
    (reconstitute_date_dmy conf (x ^ "1"), reconstitute_date_dmy conf (x ^ "2"))
  in
  let sex = match gets "sex" with "M" -> 0 | "F" -> 1 | _ -> 2 in
  (* Fonction pour tester un simple champ texte (e.g: first_name). *)
  let string_field x search =
    if test_string x then search ^ " " ^ gets x else search
  in
  (* Returns the place and date request. (e.g.: ...in Paris between 1800 and 1900) *)
  let get_place_date_request place_prefix_field_name date_prefix_field_name
      search =
    let search =
      match getd date_prefix_field_name with
      | Some d1, Some d2 ->
          Printf.sprintf "%s %s %s %s %s" search
            (transl conf "between (date)")
            (DateDisplay.string_of_dmy conf d1 :> string)
            (transl conf "and")
            (DateDisplay.string_of_dmy conf d2 :> string)
      | Some d1, _ ->
          Printf.sprintf "%s %s %s" search
            (transl conf "after (date)")
            (DateDisplay.string_of_dmy conf d1 :> string)
      | _, Some d2 ->
          Printf.sprintf "%s %s %s" search
            (transl conf "before (date)")
            (DateDisplay.string_of_dmy conf d2 :> string)
      | _ -> search
    in
    if test_string place_prefix_field_name then
      search ^ " " ^ transl conf "in (place)" ^ " "
      ^ gets place_prefix_field_name
    else search
  in
  (* Returns the event request. (e.g.: born in...) *)
  let get_event_field_request place_prefix_field_name date_prefix_field_name
      event_name search search_type =
    (* Separator character depends on search type operator, a comma for AND search, a slash for OR search. *)
    let sep =
      if search = "" then ""
      else match search_type with Fields.And -> ", " | Or -> " / "
    in
    let search =
      if test_string place_prefix_field_name || test_date date_prefix_field_name
      then search ^ sep ^ transl_nth conf event_name sex
      else search
    in
    (* The place and date have to be shown after each event only for the AND request. *)
    match search_type with
    | Fields.And ->
        get_place_date_request place_prefix_field_name date_prefix_field_name
          search
    | Or -> search
  in
  let sosa_field search =
    if gets "sosa_filter" <> "" then
      match Util.find_sosa_ref conf base with
      | None -> search
      | Some p ->
          let s =
            Printf.sprintf
              (ftransl conf "direct ancestor of %s")
              (NameDisplay.fullname_html_of_person conf base p :> string)
          in
          if search = "" then s
          else if s = "" then search
          else search ^ ", " ^ s
    else search
  in
  let search_type = get_search_type gets in
  let search = "" in
  let search = string_field "first_name" search in
  let search = string_field "surname" search in
  let search = sosa_field search in
  let build_event_search event_search (s1, s2) =
    let date_field_name =
      Fields.get_event_field_name gets "date" s1 search_type
    in
    let place_field_name =
      Fields.get_event_field_name gets "place" s1 search_type
    in
    get_event_field_request place_field_name date_field_name s2 event_search
      search_type
  in
  let events =
    [|
      ("birth", "born");
      ("bapt", "baptized");
      ("marriage", "married");
      ("death", "died");
      ("burial", "buried");
      ("other_events", "other_events");
    |]
  in
  let event_search = Array.fold_left build_event_search "" events in
  let search =
    if search = "" then event_search
    else if event_search = "" then search
    else search ^ ", " ^ event_search
  in
  (* Adding the place and date at the end for the OR request. *)
  let search =
    match search_type with
    | And -> search
    | Fields.Or ->
        if
          gets "place" != ""
          || gets "date2_yyyy" != ""
          || gets "date1_yyyy" != ""
        then get_place_date_request "place" "date" search
        else search
  in
  let search =
    let marriage_place_field_name =
      Fields.get_event_field_name gets "place" "marriage" search_type
    in
    if not (test_string marriage_place_field_name || test_date "marriage") then
      let sep = if search <> "" then ", " else "" in
      if gets "married" = "Y" then search ^ sep ^ transl conf "having a family"
      else if gets "married" = "N" then
        search ^ sep ^ transl conf "having no family"
      else search
    else search
  in
  let sep = if search <> "" then "," else "" in
  Adef.safe @@ string_field "occu" (search ^ sep)

let filter_alias ~name ~split ~matching =
  let search_list = List.map Name.lower (split name) in
  let matching = matching search_list in
  if search_list = [] then fun ~aliases:_ -> []
  else fun ~aliases ->
    List.filter_map
      (fun alias ->
        let aliases = List.map Name.lower (split alias) in
        Ext_option.return_if (matching aliases) (fun () -> alias))
      aliases

let matching_first_name_aliases ~first_name =
  filter_alias ~name:first_name ~split:Name.split_fname
    ~matching:Ext_list.is_subset

let exact_matching_first_name_aliases ~first_name =
  filter_alias ~name:first_name ~split:Name.split_fname
    ~matching:Ext_list.elements_cmp

let matching_surname_aliases ~surname =
  filter_alias ~name:surname ~split:Name.split_sname
    ~matching:Ext_list.is_subset

let exact_matching_surname_aliases ~surname =
  filter_alias ~name:surname ~split:Name.split_sname
    ~matching:Ext_list.elements_cmp
