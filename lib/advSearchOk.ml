(* Copyright (c) 1998-2007 INRIA *)

let get_number var key env = Util.p_getint env (var ^ "_" ^ key)

let reconstitute_date_dmy conf var =
  match get_number var "yyyy" conf.Config.env with
  | Some y -> (
      match get_number var "mm" conf.Config.env with
      | Some m -> (
          match get_number var "dd" conf.Config.env with
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

let string_incl =
  let memo : (string * string, bool) Hashtbl.t = Hashtbl.create 10 in
  fun x y ->
    let rec loop j_ini =
      if j_ini = String.length y then false
      else
        let rec loop1 i j =
          if i = String.length x then
            if j = String.length y then true
            else
              String.unsafe_get y j = ' ' || String.unsafe_get y (j - 1) = ' '
          else if
            j < String.length y && String.unsafe_get x i = String.unsafe_get y j
          then loop1 (i + 1) (j + 1)
          else loop (skip_spaces y (skip_no_spaces y j_ini))
        in
        loop1 0 j_ini
    in
    match Hashtbl.find_opt memo (x, y) with
    | Some b -> b
    | None ->
        let b = loop 0 in
        Hashtbl.replace memo (x, y) b;
        b

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
  type place = string * Gwdb.istr option

  val match_name :
    search_list:string list list ->
    mode:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] ->
    string list ->
    bool

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
    exact_first_name:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] ->
    exact_surname:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] ->
    bool

  val match_marriage :
    exact_place:bool ->
    conf:Config.config ->
    base:Gwdb.base ->
    p:Gwdb.person ->
    places:place list ->
    default:bool ->
    dates:Date.dmy option * Date.dmy option ->
    bool

  module type Match = sig
    val match_baptism :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_birth :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_burial :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_death :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_other_events :
      conf:Config.config ->
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool
  end

  module And : Match
  module Or : Match
end = struct
  type place = string * Gwdb.istr option

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

  let match_sex ~p ~sex =
    if sex = Def.Neuter then true else Gwdb.get_sex p = sex

  let married_cmp p = function
    | "Y" -> Gwdb.get_family p <> [||]
    | "N" -> Gwdb.get_family p = [||]
    | _ -> true

  let match_married ~p ~married =
    if married = "" then true else married_cmp p married

  let exact_place_wrapper ~get ~exact_place ~base ~p ~(places : place list)
      ~default =
    if places = [] then default
    else if exact_place then
      List.exists
        (fun (_, istr_o) ->
          match istr_o with
          | None -> false
          | Some istr -> Gwdb.compare_istr istr (get p) = 0)
        places
    else
      List.exists
        (fun (abbreved_str, _) ->
          string_incl abbreved_str (abbrev_lower (Gwdb.sou base (get p))))
        places

  let match_baptism_place = exact_place_wrapper ~get:Gwdb.get_baptism_place
  let match_birth_place = exact_place_wrapper ~get:Gwdb.get_birth_place
  let match_death_place = exact_place_wrapper ~get:Gwdb.get_death_place
  let match_burial_place = exact_place_wrapper ~get:Gwdb.get_burial_place
  let match_other_event_place = exact_place_wrapper ~get:Event.get_place
  let match_marriage_place = exact_place_wrapper ~get:Gwdb.get_marriage_place

  let match_other_events_place ~exact_place ~conf ~base ~p ~places ~default =
    if places = [] then default
    else
      List.exists
        (fun e ->
          match_other_event_place ~exact_place ~base ~places ~default:false ~p:e)
        (Event.other_events conf base p)

  let match_marriage ~exact_place ~conf ~base ~p ~places ~default ~dates =
    let d1, d2 = dates in
    let test_date_place df =
      Array.exists
        (fun ifam ->
          let fam = Gwdb.foi base ifam in
          let sp = Gwdb.poi base @@ Gutil.spouse (Gwdb.get_iper p) fam in
          if Person.is_visible conf base sp then
            df fam
            && match_marriage_place ~exact_place ~default:true ~base ~p:fam
                 ~places
          else false)
        (Gwdb.get_family p)
    in
    match (d1, d2) with
    | Some d1, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) with
            | Some d ->
                if Date.compare_dmy d d1 < 0 then false
                else if Date.compare_dmy d2 d < 0 then false
                else true
            | None -> false)
    | Some d1, None ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) with
            | Some d -> if Date.compare_dmy d d1 < 0 then false else true
            | None -> false)
    | None, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) with
            | Some d -> if Date.compare_dmy d d2 > 0 then false else true
            | None -> false)
    | None, None ->
        if places = [] then default else test_date_place (fun _ -> true)

  let match_occupation ~base ~p ~occupation =
    let clean s =
      let is_delimiter c =
        let delimiters = [ ','; ';'; '.' ] in
        List.exists (Uchar.equal c) (List.map Uchar.of_char delimiters)
      in
      Utf8.filter_map
        (function
          | `Malformed _ -> None
          | `Uchar c ->
              Ext_option.return_if (not @@ is_delimiter c) (fun () -> c))
        (Utf8.unaccent @@ Utf8.lowercase s)
    in
    occupation = ""
    || string_incl (clean occupation)
         (clean @@ Gwdb.sou base @@ Gwdb.get_occupation p)

  let match_baptism_date =
    match_date ~df:(fun p -> Date.cdate_to_dmy_opt (Gwdb.get_baptism p))

  let match_birth_date =
    match_date ~df:(fun p -> Date.cdate_to_dmy_opt (Gwdb.get_birth p))

  let match_burial_date =
    let get_burial p =
      (* TODO Date.cdate_of_burial *)
      match Gwdb.get_burial p with
      | Buried cod | Cremated cod -> Date.cdate_to_dmy_opt cod
      | UnknownBurial -> None
    in
    match_date ~df:get_burial

  let match_death_date =
    match_date ~df:(fun p -> Date.dmy_of_death (Gwdb.get_death p))

  let match_other_events_date ~conf ~base ~p ~default ~dates =
    if dates = (None, None) then default
    else
      p
      |> Event.other_events conf base
      |> List.map (fun e (* wrap value in unit -> dmy to be lazy ?*) () ->
             Date.cdate_to_dmy_opt @@ Event.get_date e)
      |> List.exists (fun event_date_f ->
             match_date ~p ~default:false ~dates ~df:(fun _ -> event_date_f ()))

  let is_subset_pfx s1 s2 =
    List.for_all
      (fun e -> List.exists (fun s -> Ext_string.start_with e 0 s) s2)
      s1

  let match_name ~search_list ~mode : string list -> bool =
    let matching : string list -> string list -> bool =
      match mode with
      | `Exact -> Ext_list.elements_cmp
      | `Not_Exact -> Ext_list.is_subset
      | `Not_Exact_Prefix -> is_subset_pfx
    in
    fun x -> List.exists (fun s -> matching s x) search_list

  let wrap_match_name ~base ~search_list ~mode ~get =
    if search_list = [] then fun _ -> true
    else
      let eq = match_name ~search_list ~mode in
      fun p -> eq (List.map Name.lower @@ Name.split @@ Gwdb.sou base @@ get p)

  let match_first_name ~base ~first_name_list ~mode =
    wrap_match_name ~base ~search_list:first_name_list ~mode
      ~get:Gwdb.get_first_name

  let match_surname ~base ~surname_list ~mode =
    wrap_match_name ~base ~search_list:surname_list ~mode ~get:Gwdb.get_surname

  let match_alias ~base ~alias_list ~mode ~kind p =
    let gets =
      let get =
        match kind with
        | `First_name -> Gwdb.get_first_names_aliases
        | `Surname -> Gwdb.get_surnames_aliases
      in
      List.map (fun alias _ -> alias) (get p)
    in
    List.exists
      (fun get -> wrap_match_name ~base ~search_list:alias_list ~get ~mode p)
      gets

  (* We use [first_name_list] as the list of aliases to search for, so
     searching for a first name will also look at first name aliases. *)
  let match_first_name_alias ~base ~first_name_list ~mode p =
    match_alias ~base ~alias_list:first_name_list ~mode ~kind:`First_name p

  let match_surname_alias ~base ~surname_list ~mode p =
    match_alias ~base ~alias_list:surname_list ~mode ~kind:`Surname p

  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status ~base ~p ~sex ~married ~occupation ~first_name_list
      ~surname_list ~skip_fname ~skip_sname ~exact_first_name ~exact_surname =
    match_sex ~p ~sex
    && (skip_fname
       || match_first_name ~base ~first_name_list ~mode:exact_first_name p
       || match_first_name_alias ~base ~first_name_list ~mode:exact_first_name p
       )
    && (skip_sname
       || match_surname ~base ~surname_list ~mode:exact_surname p
       || match_surname_alias ~base ~surname_list ~mode:exact_surname p)
    && match_married ~p ~married
    && match_occupation ~base ~p ~occupation

  module type Match = sig
    val match_baptism :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_birth :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_burial :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_death :
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool

    val match_other_events :
      conf:Config.config ->
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:place list ->
      exact_place:bool ->
      bool
  end

  module And = struct
    let match_and date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : place list) ~(exact_place : bool) =
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
        ~(places : place list) ~(exact_place : bool) =
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
  let hs = Hashtbl.create 73 in
  let hss = Hashtbl.create 73 in
  let hd = Hashtbl.create 73 in
  let getd x =
    match Hashtbl.find_opt hd x with
    | Some v -> v
    | None ->
        let v =
          ( reconstitute_date_dmy conf (x ^ "1"),
            reconstitute_date_dmy conf (x ^ "2") )
        in
        Hashtbl.add hd x v;
        v
  in
  let gets x =
    match Hashtbl.find_opt hs x with
    | Some v -> v
    | None ->
        let v =
          match Util.p_getenv conf.Config.env x with Some v -> v | None -> ""
        in
        Hashtbl.add hs x v;
        v
  in

  let exact_place = "on" = gets "exact_place" in

  let places_with_istrs =
    let memo : (string * (string * Gwdb.istr option)) list ref = ref [] in
    fun places ->
      List.map
        (fun str ->
          match List.assoc_opt str !memo with
          | Some place -> place
          | None ->
              let abbreved_str = abbrev_lower str in
              let res = (abbreved_str, Gwdb.find_opt_string_istr base str) in
              memo := (str, res) :: !memo;
              res)
        places
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
            match Util.p_getenv conf.Config.env k with
            | Some v -> loop (if v <> "" then v :: acc else acc) (i + 1)
            | None -> acc
          in
          let v = loop [] 1 in
          Hashtbl.add hss x v;
          v
  in
  let fn_list =
    List.map (fun s -> List.map Name.lower @@ Name.split s) (getss "first_name")
  in
  let sn_list =
    List.map (fun s -> List.map Name.lower @@ Name.split s) (getss "surname")
  in
  let search_type = get_search_type gets in

  let place_searched place_field =
    lazy (places_with_istrs @@ getss @@ place_field ~gets ~search_type)
  in
  let birth_place_searched = place_searched Fields.birth_place in
  let bapt_place_searched = place_searched Fields.bapt_place in
  let burial_place_searched = place_searched Fields.burial_place in
  let death_place_searched = place_searched Fields.death_place in
  let marriage_place_searched = place_searched Fields.marriage_place in
  let other_events_place_searched = place_searched Fields.other_events_place in

  let get_name_search_mode key =
    let key_pfx = key ^ "_prefix" in
    let value_pfx = gets key_pfx in
    let value = gets key in
    if value_pfx = "on" then `Not_Exact_Prefix
    else if value = "on" then `Exact
    else if value = "pfx" then `Not_Exact_Prefix
    else `Not_Exact
  in

  let match_person ?(skip_fname = false) ?(skip_sname = false)
      ((list, len) as acc) p search_type =
    let auth = Person.is_visible conf base p in
    let civil_match =
      lazy
        (AdvancedSearchMatch.match_civil_status ~base ~p
           ~sex:(gets "sex" |> sex_of_string)
           ~married:(gets "married") ~occupation:(gets "occu") ~skip_fname
           ~skip_sname ~first_name_list:fn_list ~surname_list:sn_list
           ~exact_first_name:(get_name_search_mode "exact_first_name")
           ~exact_surname:(get_name_search_mode "exact_surname"))
    in
    let pmatch =
      match search_type with
      | _ when not auth -> false
      | Fields.Or ->
          let match_f ~date_field ~place_field or_f and_f =
            or_f ~base ~p
              ~dates:(getd @@ date_field ~gets ~search_type)
              ~places:(Lazy.force place_field) ~exact_place
            && and_f ~base ~p
                 ~dates:(getd @@ date_field ~gets ~search_type)
                 ~places:(Lazy.force place_field) ~exact_place
          in
          Lazy.force civil_match
          && (getss "place" = []
              && gets "date2_yyyy" = ""
              && gets "date1_yyyy" = ""
             || match_f ~date_field:Fields.bapt_date
                  ~place_field:bapt_place_searched
                  AdvancedSearchMatch.Or.match_baptism
                  AdvancedSearchMatch.And.match_baptism
             || match_f ~date_field:Fields.birth_date
                  ~place_field:birth_place_searched
                  AdvancedSearchMatch.Or.match_birth
                  AdvancedSearchMatch.And.match_birth
             || match_f ~date_field:Fields.burial_date
                  ~place_field:burial_place_searched
                  AdvancedSearchMatch.Or.match_burial
                  AdvancedSearchMatch.And.match_burial
             || match_f ~date_field:Fields.death_date
                  ~place_field:death_place_searched
                  AdvancedSearchMatch.Or.match_death
                  AdvancedSearchMatch.And.match_death
             || AdvancedSearchMatch.match_marriage ~conf ~base ~p ~exact_place
                  ~default:false
                  ~places:(Lazy.force marriage_place_searched)
                  ~dates:(getd @@ Fields.marriage_date ~gets ~search_type)
             || match_f ~date_field:Fields.other_events_date
                  ~place_field:other_events_place_searched
                  (AdvancedSearchMatch.Or.match_other_events ~conf)
                  (AdvancedSearchMatch.And.match_other_events ~conf))
      | _ ->
          Lazy.force civil_match
          && AdvancedSearchMatch.And.match_baptism ~base ~p ~exact_place
               ~dates:(getd @@ Fields.bapt_date ~gets ~search_type)
               ~places:(Lazy.force bapt_place_searched)
          && AdvancedSearchMatch.And.match_birth ~base ~p ~exact_place
               ~dates:(getd @@ Fields.birth_date ~gets ~search_type)
               ~places:(Lazy.force birth_place_searched)
          && AdvancedSearchMatch.And.match_burial ~base ~p ~exact_place
               ~dates:(getd @@ Fields.burial_date ~gets ~search_type)
               ~places:(Lazy.force burial_place_searched)
          && AdvancedSearchMatch.And.match_death ~base ~p ~exact_place
               ~dates:(getd @@ Fields.death_date ~gets ~search_type)
               ~places:(Lazy.force death_place_searched)
          && AdvancedSearchMatch.match_marriage ~conf ~base ~p ~exact_place
               ~default:true
               ~places:(Lazy.force marriage_place_searched)
               ~dates:(getd @@ Fields.marriage_date ~gets ~search_type)
          && AdvancedSearchMatch.And.match_other_events ~conf ~base ~p
               ~exact_place
               ~dates:(getd @@ Fields.other_events_date ~gets ~search_type)
               ~places:(Lazy.force other_events_place_searched)
    in
    if pmatch then (p :: list, len + 1) else acc
  in
  let list, len =
    if "on" = gets "sosa_filter" then
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
          let rec loop p (set, acc) =
            if not (Gwdb.IperSet.mem (Gwdb.get_iper p) set) then
              let set = Gwdb.IperSet.add (Gwdb.get_iper p) set in
              let acc = match_person acc p search_type in
              match Gwdb.get_parents p with
              | Some ifam ->
                  let fam = Gwdb.foi base ifam in
                  let set, acc =
                    loop (Util.pget conf base @@ Gwdb.get_mother fam) (set, acc)
                  in
                  loop (Util.pget conf base @@ Gwdb.get_father fam) (set, acc)
              | None -> (set, acc)
            else (set, acc)
          in
          loop
            (Util.pget conf base @@ Gwdb.get_iper sosa_ref)
            (Gwdb.IperSet.empty, ([], 0))
          |> snd
      | None -> ([], 0)
    else if fn_list <> [] || sn_list <> [] then
      let list_aux strings_of persons_of n_list mode =
        List.map
          (List.map (fun x ->
               let eq =
                 AdvancedSearchMatch.match_name ~search_list:n_list ~mode
               in
               let istrs = strings_of base x in
               List.fold_left
                 (fun acc istr ->
                   let str = Mutil.nominative (Gwdb.sou base istr) in
                   if eq (List.map Name.lower @@ Name.split str) then
                     istr :: acc
                   else acc)
                 [] istrs))
          n_list
        |> List.flatten |> List.flatten |> List.sort_uniq compare
        |> List.map (Gwdb.spi_find @@ persons_of base)
        |> List.flatten |> List.sort_uniq compare
      in
      let skip_fname, skip_sname, list =
        let use_prefix_mode = gets "pfx" = "on" in
        if use_prefix_mode then
          let first_name_prefix = gets "first_name" in
          let surname_prefix = gets "surname" in
          let list =
            SearchName.persons_starting_with ~conf ~base ~first_name_prefix
              ~surname_prefix ~limit:max_answers
          in
          let list = List.map Gwdb.get_iper list in
          (true, true, list)
        else if sn_list <> [] then
          if get_name_search_mode "exact_surname" = `Not_Exact_Prefix then
            let list =
              SearchName.persons_starting_with ~conf ~base ~first_name_prefix:""
                ~surname_prefix:(gets "surname") ~limit:Int.max_int
            in
            let ipers = List.map Gwdb.get_iper list in
            (false, true, ipers)
          else
            ( false,
              true,
              list_aux Gwdb.base_strings_of_surname Gwdb.persons_of_surname
                sn_list
                (get_name_search_mode "exact_surname") )
        else if get_name_search_mode "exact_first_name" = `Not_Exact_Prefix then
          let list =
            SearchName.persons_starting_with ~conf ~base
              ~first_name_prefix:(gets "first_name") ~surname_prefix:""
              ~limit:max_answers
          in
          let ipers = List.map Gwdb.get_iper list in
          (false, true, ipers)
        else
          ( true,
            false,
            list_aux Gwdb.base_strings_of_first_name Gwdb.persons_of_first_name
              fn_list
              (get_name_search_mode "exact_first_name") )
      in
      let rec loop ((_, len) as acc) = function
        | [] -> acc
        | _ when len > max_answers -> acc
        | ip :: l ->
            loop
              (match_person ~skip_fname ~skip_sname acc (Util.pget conf base ip)
                 search_type)
              l
      in
      loop ([], 0) list
    else (
      Gwdb.load_persons_array base;
      let result =
        Gwdb.Collection.fold_until
          (fun (_, len) -> len <= max_answers)
          (fun acc i -> match_person acc (Util.pget conf base i) search_type)
          ([], 0) (Gwdb.ipers base)
      in
      Gwdb.clear_persons_array base;
      result)
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
    match Util.p_getenv conf.Config.env x with
    | Some v when v <> "" -> v
    | _ ->
        let rec loop acc i =
          let k = x ^ "_" ^ string_of_int i in
          match Util.p_getenv conf.Config.env k with
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
            (Util.transl conf "between (date)")
            (DateDisplay.string_of_dmy conf d1 :> string)
            (Util.transl conf "and")
            (DateDisplay.string_of_dmy conf d2 :> string)
      | Some d1, _ ->
          Printf.sprintf "%s %s %s" search
            (Util.transl conf "after (date)")
            (DateDisplay.string_of_dmy conf d1 :> string)
      | _, Some d2 ->
          Printf.sprintf "%s %s %s" search
            (Util.transl conf "before (date)")
            (DateDisplay.string_of_dmy conf d2 :> string)
      | _ -> search
    in
    if test_string place_prefix_field_name then
      search ^ " "
      ^ Util.transl conf "in (place)"
      ^ " "
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
      then search ^ sep ^ Util.transl_nth conf event_name sex
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
              (Util.ftransl conf "direct ancestor(s) of %s")
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
      if gets "married" = "Y" then
        search ^ sep ^ Util.transl conf "having a family"
      else if gets "married" = "N" then
        search ^ sep ^ Util.transl conf "having no family"
      else search
    else search
  in
  let sep = if search <> "" then "," else "" in
  Adef.safe @@ string_field "occu" (search ^ sep)

let filter_alias ~name ~matching =
  let search_list = List.map Name.lower (Name.split name) in
  let matching = matching search_list in
  if search_list = [] then fun ~aliases:_ -> []
  else fun ~aliases ->
    List.filter_map
      (fun alias ->
        let aliases = List.map Name.lower (Name.split alias) in
        Ext_option.return_if (matching aliases) (fun () -> alias))
      aliases

let matching_first_name_aliases ~first_name =
  filter_alias ~name:first_name ~matching:Ext_list.is_subset

let exact_matching_first_name_aliases ~first_name =
  filter_alias ~name:first_name ~matching:Ext_list.elements_cmp

let matching_surname_aliases ~surname =
  filter_alias ~name:surname ~matching:Ext_list.is_subset

let exact_matching_surname_aliases ~surname =
  filter_alias ~name:surname ~matching:Ext_list.elements_cmp
