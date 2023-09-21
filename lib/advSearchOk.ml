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

  module OR : sig
    val date : name
    val place : name
  end

  module AND : sig
    val bapt_date : name
    val birth_date : name
    val death_date : name
    val burial_date : name
    val marriage_date : name
    val other_events_date : name
    val bapt_place : name
    val birth_place : name
    val death_place : name
    val burial_place : name
    val marriage_place : name
    val other_events_place : name
  end
end = struct
  type search = And | Or
  type name = string

  module OR = struct
    let place = "place"
    let date = "date"
  end

  module AND = struct
    let field_base criteria event = event ^ "_" ^ criteria
    let date_field = field_base "date"
    let place_field = field_base "place"
    let bapt_date = date_field "bapt"
    let birth_date = date_field "birth"
    let death_date = date_field "death"
    let burial_date = date_field "burial"
    let marriage_date = date_field "marriage"
    let other_events_date = date_field "other_events"
    let bapt_place = place_field "bapt"
    let birth_place = place_field "birth"
    let death_place = place_field "death"
    let burial_place = place_field "burial"
    let marriage_place = place_field "marriage"
    let other_events_place = place_field "other_events"
  end
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
    alias_list:string list list ->
    skip_fname:bool ->
    skip_sname:bool ->
    skip_alias:bool ->
    exact_first_name:bool ->
    exact_surname:bool ->
    exact_alias:bool ->
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
  let match_date ~p ~df ~default ~dates =
    (* df gives the date of the event *)
    (* d1,d2 are the date range in which the event should be *)
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

  let do_compare ~places ~value ~cmp =
    (* value is person/familly baptism/death/... place *)
    (* places are the places we search for *)
    let places = List.map abbrev_lower places in
    List.exists (fun place -> cmp place value) places

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

  let match_baptism_place ~base ~p ~places ~cmp =
    do_compare ~places ~cmp ~value:(sou base @@ get_baptism_place p)

  let match_birth_place ~base ~p ~places ~cmp =
    do_compare ~places ~cmp ~value:(sou base @@ get_birth_place p)

  let match_death_place ~base ~p ~places ~cmp =
    do_compare ~places ~cmp ~value:(sou base @@ get_death_place p)

  let match_burial_place ~base ~p ~places ~cmp =
    do_compare ~places ~cmp ~value:(sou base @@ get_burial_place p)

  let match_other_events_place ~base ~p ~places ~cmp =
    let pevents = Gwdb.get_pevents p in
    let fevents =
      List.flatten @@ Array.to_list
      @@ Array.map Gwdb.get_fevents
           (Array.map (Gwdb.foi base) (Gwdb.get_family p))
    in
    (* wrap value in unit -> string to be lazy ?*)
    let pevent_places =
      List.map (fun e () -> sou base @@ Gwdb.get_pevent_place e) pevents
    in
    let fevent_places =
      List.map (fun e () -> sou base @@ Gwdb.get_fevent_place e) fevents
    in
    let event_places = pevent_places @ fevent_places in
    List.exists
      (fun value_f -> do_compare ~places ~cmp ~value:(value_f ()))
      event_places

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
               || do_compare ~places
                    ~value:(sou base @@ get_marriage_place fam)
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
            | Some d when authorized_age conf base p ->
                if Date.compare_dmy d d1 < 0 then false else true
            | Some _ | None -> false)
    | None, Some d2 ->
        test_date_place (fun fam ->
            match Date.cdate_to_dmy_opt (get_marriage fam) with
            | Some d when authorized_age conf base p ->
                if Date.compare_dmy d d2 > 0 then false else true
            | Some _ | None -> false)
    | None, None ->
        if places = [] then default else test_date_place (fun _ -> true)

  let match_marriage = exact_place_wrapper match_marriage

  let match_occupation ~base ~p ~occupation =
    if occupation = "" then true
    else
      string_incl (abbrev_lower occupation)
        (abbrev_lower @@ sou base @@ get_occupation p)

  let match_baptism_date ~base:_ =
    match_date ~df:(fun p -> Date.cdate_to_dmy_opt (get_birth p))

  let match_birth_date ~base:_ =
    match_date ~df:(fun p -> Date.cdate_to_dmy_opt (get_birth p))

  let match_burial_date ~base:_ =
    let get_burial p =
      match get_burial p with
      | Buried cod | Cremated cod -> Date.cdate_to_dmy_opt cod
      | _ -> None
    in
    match_date ~df:get_burial

  let match_death_date ~base:_ =
    match_date ~df:(fun p -> Date.dmy_of_death (get_death p))

  let match_other_events_date ~base ~p ~default ~dates =
    let pevents = Gwdb.get_pevents p in
    let fevents =
      List.flatten @@ Array.to_list
      @@ Array.map Gwdb.get_fevents
           (Array.map (Gwdb.foi base) (Gwdb.get_family p))
    in
    (* wrap value in unit -> dmy to be lazy ?*)
    let pevent_dates =
      List.map
        (fun e () -> Date.cdate_to_dmy_opt @@ Gwdb.get_pevent_date e)
        pevents
    in
    let fevent_dates =
      List.map
        (fun e () -> Date.cdate_to_dmy_opt @@ Gwdb.get_fevent_date e)
        fevents
    in
    let event_dates = pevent_dates @ fevent_dates in
    List.exists
      (fun event_date_f ->
        match_date ~p ~default ~dates ~df:(fun _p -> event_date_f ()))
      event_dates

  let match_name ~search_list ~exact : string list -> bool =
    let eq : string list -> string list -> bool =
      if exact then fun x search ->
        List.sort compare search = List.sort compare x
      else fun x search -> List.for_all (fun s -> List.mem s x) search
    in
    fun x -> List.exists (eq x) search_list

  let wrap_match_name ~base ~search_list ~get ~exact ~split =
    match_name ~search_list ~exact
      (List.map Name.lower @@ split @@ sou base @@ get ())

  let match_first_name ~base ~first_name_list ~exact ~p =
    wrap_match_name ~base ~search_list:first_name_list
      ~get:(fun () -> get_first_name p)
      ~exact ~split:Name.split_fname

  (* we use [first_name_list] as the list of aliases to search for.
     so searching for a first name will also look at first name aliases *)
  let match_first_names_aliases ~base ~first_name_list ~exact ~p =
    let gets =
      List.map (fun alias () -> alias) (Gwdb.get_first_names_aliases p)
    in
    List.exists
      (fun get ->
        wrap_match_name ~base ~search_list:first_name_list ~get ~exact
          ~split:Name.split_fname)
      gets

  let match_surname ~base ~surname_list ~exact ~p =
    wrap_match_name ~base ~search_list:surname_list
      ~get:(fun () -> get_surname p)
      ~exact ~split:Name.split_sname

  let match_surnames_aliases ~base ~surname_list ~exact ~p =
    let gets = List.map (fun alias () -> alias) (Gwdb.get_surnames_aliases p) in
    List.exists
      (fun get ->
        wrap_match_name ~base ~search_list:surname_list ~get ~exact
          ~split:Name.split_sname)
      gets

  let match_alias ~base ~alias_list ~exact ~p =
    let gets = List.map (fun alias () -> alias) (Gwdb.get_aliases p) in
    List.exists
      (fun get ->
        wrap_match_name ~base ~search_list:alias_list ~get ~exact
          ~split:(*TODO which split to use?? *) Name.split_sname)
      gets

  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status ~base ~p ~sex ~married ~occupation ~first_name_list
      ~surname_list ~alias_list ~skip_fname ~skip_sname ~skip_alias
      ~exact_first_name ~exact_surname ~exact_alias =
    match_sex ~p ~sex
    && (skip_fname || first_name_list = []
       || match_first_name ~base ~first_name_list ~exact:exact_first_name ~p
       || match_first_names_aliases ~base ~first_name_list
            ~exact:exact_first_name ~p)
    && (skip_sname || surname_list = []
       || match_surname ~base ~surname_list ~exact:exact_surname ~p
       || match_surnames_aliases ~base ~surname_list ~exact:exact_surname ~p)
    && (skip_alias || alias_list = []
       || match_alias ~base ~alias_list ~exact:exact_alias ~p)
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
      base:Gwdb.base ->
      p:Gwdb.person ->
      dates:Date.dmy option * Date.dmy option ->
      places:string list ->
      exact_place:bool ->
      bool
  end

  module And = struct
    let default = true

    let match_and date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : string list) ~(exact_place : bool) =
      let cmp = if exact_place then ( = ) else string_incl in
      let place_f =
        if places = [] then fun ~base:_ ~p:_ ~places:_ ~cmp:_ -> default
        else place_f
      in
      date_f ~base ~p ~default ~dates && place_f ~base ~p ~places ~cmp

    let match_baptism = match_and match_baptism_date match_baptism_place
    let match_birth = match_and match_birth_date match_birth_place
    let match_burial = match_and match_burial_date match_burial_place
    let match_death = match_and match_death_date match_death_place

    let match_other_events =
      match_and match_other_events_date match_other_events_place
  end

  module Or = struct
    let default = false

    let match_or date_f place_f ~(base : Gwdb.base) ~p ~dates
        ~(places : string list) ~(exact_place : bool) =
      let cmp = if exact_place then ( = ) else string_incl in
      let place_f =
        if places = [] then fun ~base:_ ~p:_ ~places:_ ~cmp:_ -> default
        else place_f
      in
      date_f ~base ~p ~default ~dates || place_f ~base ~p ~places ~cmp

    let match_baptism = match_or match_baptism_date match_baptism_place
    let match_birth = match_or match_birth_date match_birth_place
    let match_burial = match_or match_burial_date match_burial_place
    let match_death = match_or match_death_date match_death_place

    let match_other_events =
      match_or match_other_events_date match_other_events_place
  end
end

(* Get the field name of an event criteria depending of the search type. *)
let get_event_field_name gets event_criteria event_name search_type =
  match search_type with
  | Fields.And -> event_name ^ "_" ^ event_criteria
  | Or -> if "on" = gets ("event_" ^ event_name) then event_criteria else ""

(* Search type can be AND or OR. *)
let get_search_type gets =
  match gets "search_type" with
  | "AND" -> Fields.And
  | "OR" -> Fields.Or
  | s -> failwith @@ "unsupported advanced search mode : " ^ s

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
      match Hashtbl.find_opt hss @@ x with
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
  let alias_list =
    List.map
      (fun s ->
        List.map Name.lower
        @@ (* TODO which spil function to use here *) Name.split_sname s)
      (getss "alias")
  in
  let search_type = get_search_type gets in

  let exact_place = "on" = gets "exact_place" in

  let match_person ?(skip_fname = false) ?(skip_sname = false)
      ?(skip_alias = false) ((list, len) as acc) p search_type =
    let auth = authorized_age conf base p in

    let civil_match =
      lazy
        (match_civil_status ~base ~p
           ~sex:(gets "sex" |> sex_of_string)
           ~married:(gets "married") ~occupation:(gets "occu") ~skip_fname
           ~skip_sname ~skip_alias ~first_name_list:fn_list
           ~surname_list:sn_list ~alias_list
           ~exact_first_name:(gets "exact_first_name" = "on")
           ~exact_surname:(gets "exact_surname" = "on")
           ~exact_alias:(gets "exact_alias" = "on"))
    in

    let pmatch =
      match search_type with
      | _ when not auth -> false
      | Fields.And ->
          Lazy.force civil_match
          && And.match_baptism ~base ~p ~exact_place
               ~dates:(getd Fields.AND.bapt_date)
               ~places:(getss Fields.AND.bapt_place)
          && And.match_birth ~base ~p ~exact_place
               ~dates:(getd Fields.AND.birth_date)
               ~places:(getss Fields.AND.birth_place)
          && And.match_burial ~base ~p ~exact_place
               ~dates:(getd Fields.AND.burial_date)
               ~places:(getss Fields.AND.burial_place)
          && And.match_death ~base ~p ~exact_place
               ~dates:(getd Fields.AND.death_date)
               ~places:(getss Fields.AND.death_place)
          && match_marriage ~conf ~base ~p ~exact_place ~default:true
               ~places:(getss Fields.AND.marriage_place)
               ~dates:(getd Fields.AND.marriage_date)
          && And.match_other_events ~base ~p ~exact_place
               ~dates:(getd Fields.AND.other_events_date)
               ~places:(getss Fields.AND.other_events_place)
      | Fields.Or ->
          let match_f or_f and_f =
            or_f ~base ~p ~dates:(getd Fields.OR.date)
              ~places:(getss Fields.OR.place) ~exact_place
            && and_f ~base ~p ~dates:(getd Fields.OR.date)
                 ~places:(getss Fields.OR.place) ~exact_place
          in
          Lazy.force civil_match
          && (getss "place" = []
              && gets "date2_yyyy" = ""
              && gets "date1_yyyy" = ""
             || match_f Or.match_baptism And.match_baptism
             || match_f Or.match_birth And.match_birth
             || match_f Or.match_burial And.match_burial
             || match_f Or.match_death And.match_death
             || match_marriage ~conf ~base ~p ~exact_place ~default:false
                  ~places:(getss Fields.OR.place) ~dates:(getd Fields.OR.date))
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
      if search <> "" then if search_type <> Fields.Or then ", " else " / "
      else ""
    in
    let search =
      if test_string place_prefix_field_name || test_date date_prefix_field_name
      then search ^ sep ^ transl_nth conf event_name sex
      else search
    in
    (* The place and date have to be shown after each event only for the AND request. *)
    if search_type <> Or then
      get_place_date_request place_prefix_field_name date_prefix_field_name
        search
    else search
  in
  let sosa_field search =
    if gets "sosa_filter" <> "" then
      match Util.find_sosa_ref conf base with
      | Some p ->
          let s =
            Printf.sprintf
              (ftransl conf "direct ancestor of %s")
              (Util.gen_person_text conf base p :> string (* TODO check this *))
          in
          if search = "" then s
          else if s = "" then search
          else search ^ ", " ^ s
      | None -> search
    else search
  in
  (* Search type can be AND or OR. *)
  let search_type = get_search_type gets in
  let search = "" in
  let search = string_field "first_name" search in
  let search = string_field "surname" search in
  let search = sosa_field search in
  let build_event_search event_search (s1, s2) =
    let burial_date_field_name =
      get_event_field_name gets "date" s1 search_type
    in
    let burial_place_field_name =
      get_event_field_name gets "place" s1 search_type
    in
    get_event_field_request burial_place_field_name burial_date_field_name s2
      event_search search_type
  in
  let events =
    [|
      ("burial", "buried");
      ("death", "died");
      ("marriage", "married");
      ("bapt", "baptized");
      ("birth", "born");
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
    | Fields.Or ->
        if
          gets "place" != ""
          || gets "date2_yyyy" != ""
          || gets "date1_yyyy" != ""
        then get_place_date_request "place" "date" search
        else search
    | And -> search
  in
  let search =
    let marriage_place_field_name =
      get_event_field_name gets "place" "marriage" search_type
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
