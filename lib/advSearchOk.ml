(* Copyright (c) 1998-2007 INRIA *)

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

let is_subset_pfx s1 s2 =
  List.for_all
    (fun e -> List.exists (fun s -> Ext_string.start_with e 0 s) s2)
    s1

module AdvancedSearchMatch : sig
  type place = string * Gwdb.istr option

  val match_name :
    search_list:string list ->
    mode:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] ->
    string list ->
    bool

  val match_civil_status :
    conf:Config.config ->
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    sex:Def.sex ->
    married:bool option ->
    occupation:string option ->
    first_name_list:string list ->
    surname_list:string list ->
    alias_public_name_qualifiers:string list ->
    skip_fname:bool ->
    skip_sname:bool ->
    exact_first_name:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] ->
    exact_surname:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] ->
    bool

  val match_marriage :
    default:bool ->
    conf:Config.config ->
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    dates:Date.dmy option * Date.dmy option ->
    place:place option ->
    exact_place:bool ->
    bool

  val match_baptism :
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    dates:Date.dmy option * Date.dmy option ->
    place:place option ->
    exact_place:bool ->
    bool

  val match_birth :
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    dates:Date.dmy option * Date.dmy option ->
    place:place option ->
    exact_place:bool ->
    bool

  val match_burial :
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    dates:Date.dmy option * Date.dmy option ->
    place:place option ->
    exact_place:bool ->
    bool

  val match_death :
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    dates:Date.dmy option * Date.dmy option ->
    place:place option ->
    exact_place:bool ->
    bool

  val match_other_events :
    conf:Config.config ->
    base:Gwdb.base ->
    p:Authorized.Person.t ->
    dates:Date.dmy option * Date.dmy option ->
    place:place option ->
    exact_place:bool ->
    bool
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
    sex = Def.Neuter || Authorized.Person.get_sex p = Some sex

  let married_cmp ~conf ~base p married =
    if married then
      Option.fold ~none:false
        (Authorized.Person.get_family ~conf ~base p)
        ~some:(( <> ) [||])
    else
      Option.fold ~none:false
        (Authorized.Person.get_family ~conf ~base p)
        ~some:(( = ) [||])

  let match_married ~conf ~base ~p ~married =
    Option.fold married ~none:true ~some:(married_cmp ~conf ~base p)

  let exact_place_wrapper ~get ~exact_place ~base ~p ~(place : place option)
      ~default =
    Option.fold place ~none:default ~some:(fun place ->
        if exact_place then
          let _, istr_o = place in
          match istr_o with
          | None -> false
          | Some istr ->
              Option.fold ~none:false
                ~some:(fun istr' -> Gwdb.compare_istr istr istr' = 0)
                (get p)
        else
          let abbreved_str, _ = place in
          Option.fold ~none:false
            ~some:(fun istr' ->
              string_incl abbreved_str (abbrev_lower (Gwdb.sou base istr')))
            (get p))

  let match_baptism_place =
    exact_place_wrapper ~get:Authorized.Person.get_baptism_place

  let match_birth_place =
    exact_place_wrapper ~get:Authorized.Person.get_birth_place

  let match_death_place =
    exact_place_wrapper ~get:Authorized.Person.get_death_place

  let match_burial_place =
    exact_place_wrapper ~get:Authorized.Person.get_burial_place

  let match_other_event_place =
    exact_place_wrapper ~get:(fun event -> Some (Event.get_place event))

  let match_marriage_place =
    exact_place_wrapper ~get:Authorized.Family.get_marriage_place

  let match_other_events_place ~exact_place ~conf ~base ~p ~place ~default =
    if place = None then default
    else
      List.exists
        (fun e ->
          match_other_event_place ~exact_place ~base ~place ~default:false ~p:e)
        (Event.other_events conf base p)

  let match_marriage ~default ~conf ~base ~p ~dates ~place ~exact_place =
    let d1, d2 = dates in
    let test_date_place df =
      Array.exists
        (fun fam ->
          df fam
          && match_marriage_place ~exact_place ~default:true ~base ~p:fam ~place)
        (Option.value ~default:[||]
           (Authorized.Person.get_family ~conf ~base p))
    in
    match (d1, d2) with
    | Some d1, Some d2 ->
        test_date_place (fun fam ->
            match
              Option.bind
                (Authorized.Family.get_marriage fam)
                Date.cdate_to_dmy_opt
            with
            | Some d -> Date.compare_dmy d d1 >= 0 && Date.compare_dmy d2 d >= 0
            | None -> false)
    | Some d1, None ->
        test_date_place (fun fam ->
            match
              Option.bind
                (Authorized.Family.get_marriage fam)
                Date.cdate_to_dmy_opt
            with
            | Some d -> Date.compare_dmy d d1 >= 0
            | None -> false)
    | None, Some d2 ->
        test_date_place (fun fam ->
            match
              Option.bind
                (Authorized.Family.get_marriage fam)
                Date.cdate_to_dmy_opt
            with
            | Some d -> Date.compare_dmy d d2 <= 0
            | None -> false)
    | None, None ->
        if place = None then default else test_date_place (fun _ -> true)

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
    Option.fold occupation ~none:true ~some:(fun occupation ->
        Option.fold ~none:false
          ~some:(fun occupation' ->
            string_incl (clean occupation) (clean @@ Gwdb.sou base occupation'))
          (Authorized.Person.get_occupation p))

  let match_baptism_date =
    match_date ~df:(fun p ->
        Option.bind (Authorized.Person.get_baptism p) Date.cdate_to_dmy_opt)

  let match_birth_date =
    match_date ~df:(fun p ->
        Option.bind (Authorized.Person.get_birth p) Date.cdate_to_dmy_opt)

  let match_burial_date =
    let get_burial p =
      (* TODO Date.cdate_of_burial *)
      match Authorized.Person.get_burial p with
      | Some (Buried cod | Cremated cod) -> Date.cdate_to_dmy_opt cod
      | Some UnknownBurial | None -> None
    in
    match_date ~df:get_burial

  let match_death_date =
    match_date ~df:(fun p ->
        Option.bind (Authorized.Person.get_death p) Date.dmy_of_death)

  let match_other_events_date ~conf ~base ~p ~default ~dates =
    if dates = (None, None) then default
    else
      p
      |> Event.other_events conf base
      |> List.map (fun e (* wrap value in unit -> dmy to be lazy ?*) () ->
             Date.cdate_to_dmy_opt @@ Event.get_date e)
      |> List.exists (fun event_date_f ->
             match_date ~p ~default:false ~dates ~df:(fun _ -> event_date_f ()))

  let match_name ~search_list ~mode : string list -> bool =
    let matching : string list -> string list -> bool =
      match mode with
      | `Exact -> Ext_list.elements_cmp
      | `Not_Exact -> Ext_list.is_subset
      | `Not_Exact_Prefix -> is_subset_pfx
    in
    fun x -> matching search_list x

  let wrap_match_name ~base ~search_list ~mode ~get =
    if search_list = [] then fun _ -> true
    else
      let eq = match_name ~search_list ~mode in
      fun p ->
        eq
          (Option.fold ~none:[]
             ~some:(fun name ->
               List.map Name.lower @@ Name.split @@ Gwdb.sou base name)
             (get p))

  let match_first_name ~base ~first_name_list ~mode =
    wrap_match_name ~base ~search_list:first_name_list ~mode
      ~get:Authorized.Person.get_first_name

  let match_surname ~base ~surname_list ~mode =
    wrap_match_name ~base ~search_list:surname_list ~mode
      ~get:Authorized.Person.get_surname

  let match_alias ~base ~alias_list ~mode ~kind p =
    let gets =
      let get =
        match kind with
        | `First_name -> Authorized.Person.get_first_names_aliases
        | `Surname -> Authorized.Person.get_surnames_aliases
        | `Alias -> Authorized.Person.get_aliases
        | `Public_name ->
            fun p ->
              Option.map
                (fun name -> [ name ])
                (Authorized.Person.get_public_name p)
        | `Qualifiers -> Authorized.Person.get_qualifiers
      in
      List.map (fun alias _ -> Some alias) (Option.value ~default:[] (get p))
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

  let match_alias_public_name_qualifiers ~base ~alias_public_name_qualifiers p =
    alias_public_name_qualifiers = []
    || match_alias ~base ~alias_list:alias_public_name_qualifiers ~mode:`Exact
         ~kind:`Alias p
    || match_alias ~base ~alias_list:alias_public_name_qualifiers ~mode:`Exact
         ~kind:`Public_name p
    || match_alias ~base ~alias_list:alias_public_name_qualifiers ~mode:`Exact
         ~kind:`Qualifiers p

  (* Check the civil status. The test is the same for an AND or a OR search request. *)
  let match_civil_status ~conf ~base ~p ~sex ~married ~occupation
      ~first_name_list ~surname_list ~alias_public_name_qualifiers ~skip_fname
      ~skip_sname ~exact_first_name ~exact_surname =
    match_sex ~p ~sex
    && (skip_fname
       || match_first_name ~base ~first_name_list ~mode:exact_first_name p
       || match_first_name_alias ~base ~first_name_list ~mode:exact_first_name p
       )
    && (skip_sname
       || match_surname ~base ~surname_list ~mode:exact_surname p
       || match_surname_alias ~base ~surname_list ~mode:exact_surname p)
    && match_alias_public_name_qualifiers ~base ~alias_public_name_qualifiers p
    && match_married ~conf ~base ~p ~married
    && match_occupation ~base ~p ~occupation

  let match_and date_f place_f ~(base : Gwdb.base) ~p ~dates
      ~(place : place option) ~(exact_place : bool) =
    date_f ~p ~default:true ~dates
    && place_f ~exact_place ~base ~p ~place ~default:true

  let match_baptism = match_and match_baptism_date match_baptism_place
  let match_birth = match_and match_birth_date match_birth_place
  let match_burial = match_and match_burial_date match_burial_place
  let match_death = match_and match_death_date match_death_place

  let match_other_events ~conf ~base =
    match_and ~base
      (match_other_events_date ~conf ~base)
      (match_other_events_place ~conf)
end

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
let advanced_search ~(query_params : Page.Advanced_search.Query_params.t) conf
    base =
  let max_answers = Option.value ~default:max_int query_params.limit in
  let place_with_istr =
    let memo : (string * (string * Gwdb.istr option)) list ref = ref [] in
    fun str ->
      match List.assoc_opt str !memo with
      | Some place -> place
      | None ->
          let abbreved_str = abbrev_lower str in
          let res = (abbreved_str, Gwdb.find_opt_string_istr base str) in
          memo := (str, res) :: !memo;
          res
  in

  let fn_list =
    List.map Name.lower
    @@ Option.fold ~none:[] ~some:Name.split query_params.first_name
  in
  let sn_list =
    List.map Name.lower
    @@ Option.fold ~none:[] ~some:Name.split query_params.surname
  in

  let alias_public_name_qualifiers =
    List.map Name.lower
    @@ Option.fold ~none:[] ~some:Name.split query_params.alias
  in

  let search_type = query_params.event_search_mode in

  let place_searched event_kind =
    lazy
      (Option.map place_with_istr
         (Page.Advanced_search.Query_params.get_event_place ~event_kind
            query_params))
  in

  let match_person ?(skip_fname = false) ?(skip_sname = false)
      ((list, len) as acc) unsafe_p search_type =
    let pmatch () =
      let p = Authorized.Person.make ~conf ~base (Gwdb.get_iper unsafe_p) in
      let civil_match =
        lazy
          (AdvancedSearchMatch.match_civil_status ~conf ~base ~p
             ~sex:query_params.sex ~married:query_params.married
             ~occupation:query_params.occupation ~skip_fname ~skip_sname
             ~first_name_list:fn_list ~surname_list:sn_list
             ~alias_public_name_qualifiers
             ~exact_first_name:query_params.first_name_search_mode
             ~exact_surname:query_params.surname_search_mode)
      in
      let check, default =
        match search_type with
        | `Or -> (List.exists, false)
        | `And -> (List.for_all, true)
      in
      let match_ (f, event_kind) =
        query_params.events = []
        || f ~base ~p
             ~dates:
               (Page.Advanced_search.Query_params.get_event_dates ~event_kind
                  query_params)
             ~place:(Lazy.force @@ place_searched event_kind)
             ~exact_place:query_params.event_exact_place
      in
      Lazy.force civil_match
      && check match_
           [
             (AdvancedSearchMatch.match_baptism, `Baptism);
             (AdvancedSearchMatch.match_birth, `Birth);
             (AdvancedSearchMatch.match_burial, `Burial);
             (AdvancedSearchMatch.match_death, `Death);
             (AdvancedSearchMatch.match_marriage ~default ~conf, `Marriage);
             (AdvancedSearchMatch.match_other_events ~conf, `Other);
           ]
    in
    if (not @@ SearchName.search_reject_p conf base unsafe_p) && pmatch () then
      (unsafe_p :: list, len + 1)
    else acc
  in
  let list, len =
    if query_params.only_root_ancestors then
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
          let rec loop p (set, acc) =
            if not (Gwdb.IperSet.mem (Gwdb.get_iper p) set) then
              let set = Gwdb.IperSet.add (Gwdb.get_iper p) set in
              let acc = match_person acc p query_params.event_search_mode in
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
          (fun x ->
            let eq = AdvancedSearchMatch.match_name ~search_list:n_list ~mode in
            let istrs = strings_of base x in
            List.fold_left
              (fun acc istr ->
                let str = Mutil.nominative (Gwdb.sou base istr) in
                if eq (List.map Name.lower @@ Name.split str) then istr :: acc
                else acc)
              [] istrs)
          n_list
        |> List.flatten |> List.sort_uniq compare
        |> List.map (Gwdb.spi_find @@ persons_of base)
        |> List.flatten |> List.sort_uniq compare
      in
      if sn_list <> [] && query_params.surname_search_mode = `Not_Exact_Prefix
      then
        let filter p =
          let r =
            match_person ~skip_fname:false ~skip_sname:true ([], 0) p
              search_type
          in
          r <> ([], 0)
        in
        let list =
          SearchName.persons_starting_with ~conf ~base ~filter
            ~first_name_prefix:""
            ~surname_prefix:(Option.value ~default:"" query_params.surname)
            ~limit:max_answers
        in
        (List.map (Gwdb.poi base) list, List.length list)
      else if
        fn_list <> [] && query_params.first_name_search_mode = `Not_Exact_Prefix
      then
        let filter p =
          let r =
            match_person ~skip_fname:true ~skip_sname:false ([], 0) p
              search_type
          in
          r <> ([], 0)
        in
        let list =
          SearchName.persons_starting_with ~conf ~base ~filter
            ~first_name_prefix:
              (Option.value ~default:"" query_params.first_name)
            ~surname_prefix:"" ~limit:max_answers
        in
        (List.map (Gwdb.poi base) list, List.length list)
      else
        let skip_fname, skip_sname, list =
          if sn_list <> [] then
            ( false,
              true,
              list_aux Gwdb.base_strings_of_surname Gwdb.persons_of_surname
                sn_list query_params.surname_search_mode )
          else
            ( true,
              false,
              list_aux Gwdb.base_strings_of_first_name
                Gwdb.persons_of_first_name fn_list
                query_params.first_name_search_mode )
        in
        let rec loop ((_, len) as acc) = function
          | [] -> acc
          | _ when len >= max_answers -> acc
          | ip :: l ->
              loop
                (match_person ~skip_fname ~skip_sname acc
                   (Util.pget conf base ip) search_type)
                l
        in
        loop ([], 0) list
    else (
      Gwdb.load_persons_array base;
      let result =
        Gwdb.Collection.fold_until
          (fun (_, len) -> len < max_answers)
          (fun acc i -> match_person acc (Util.pget conf base i) search_type)
          ([], 0) (Gwdb.ipers base)
      in
      Gwdb.clear_persons_array base;
      result)
  in
  (List.rev list, len)

module SearchingFields : sig
  val map_field :
    search_mode:[ `Exact | `Not_Exact | `Not_Exact_Prefix ] -> string -> string

  val string_field : ?map_field:(string -> string) -> string -> string -> string

  val sosa :
    query_params:Page.Advanced_search.Query_params.t ->
    Config.config ->
    Gwdb.base ->
    string

  val sosa_field :
    query_params:Page.Advanced_search.Query_params.t ->
    Config.config ->
    Gwdb.base ->
    string ->
    string

  val get_place_date_request :
    Config.config ->
    string ->
    Date.dmy option * Date.dmy option ->
    string ->
    string

  val test_string : string -> bool
  val test_date : Date.dmy option * Date.dmy option -> bool

  val event_search :
    query_params:Page.Advanced_search.Query_params.t -> Config.config -> string

  val sex : Page.Advanced_search.Query_params.t -> int
  val union : Page.Advanced_search.Query_params.t -> int
  val first_name : Page.Advanced_search.Query_params.t -> string
  val surname : Page.Advanced_search.Query_params.t -> string
  val occupation : Page.Advanced_search.Query_params.t -> string

  val events :
    query_params:Page.Advanced_search.Query_params.t -> Config.config -> string

  val other_aliases : Page.Advanced_search.Query_params.t -> string
end = struct
  let test_string x = x <> ""
  let test_date (date1, date2) = date1 <> None || date2 <> None

  (* Fonction pour tester un simple champ texte (e.g: first_name). *)
  let string_field ?(map_field = Fun.id) x search =
    if test_string x then search ^ " " ^ map_field x else search

  let sex (query_params : Page.Advanced_search.Query_params.t) =
    match query_params.sex with Male -> 0 | Female -> 1 | Neuter -> 2

  (* Returns the place and date request. (e.g.: ...in Paris between 1800 and 1900) *)
  let get_place_date_request conf place date search =
    let search =
      match date with
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
      | None, None -> search
    in
    if test_string place then
      search ^ " " ^ Util.transl conf "in (place)" ^ " " ^ place
    else search

  let events =
    [|
      (`Birth, "born");
      (`Baptism, "baptized");
      (`Marriage, "married");
      (`Death, "died");
      (`Burial, "buried");
      (`Other, "other_events");
    |]

  (* Returns the event request. (e.g.: born in...) *)
  let get_event_field_request conf place date event_name search search_type sex
      =
    (* Separator character depends on search type operator, a comma for AND search, a slash for OR search. *)
    let sep =
      if search = "" then ""
      else match search_type with `And -> ", " | `Or -> " / "
    in
    let search =
      if test_string place || test_date date then
        search ^ sep ^ Util.transl_nth conf event_name sex
      else search
    in
    (* The place and date have to be shown after each event only for the AND request. *)
    match search_type with
    | `And -> get_place_date_request conf place date search
    | `Or -> search

  let build_event_search ~query_params conf event_search (event_kind, s2) =
    let date =
      Page.Advanced_search.Query_params.get_event_dates ~event_kind query_params
    in
    let place =
      Page.Advanced_search.Query_params.get_event_place ~event_kind query_params
    in
    get_event_field_request conf
      (Option.value ~default:"" place)
      date s2 event_search query_params.event_search_mode (sex query_params)

  let event_search ~query_params conf =
    Array.fold_left (build_event_search ~query_params conf) "" events

  let sosa ~(query_params : Page.Advanced_search.Query_params.t) conf base =
    if query_params.only_root_ancestors then
      match Util.find_sosa_ref conf base with
      | None -> ""
      | Some p ->
          Printf.sprintf
            (Util.ftransl conf "direct ancestor(s) of %s")
            (NameDisplay.fullname_html_of_person conf base p :> string)
    else ""

  let sosa_field ~query_params conf base search =
    let s = sosa ~query_params conf base in
    if search = "" then s else if s = "" then search else search ^ ", " ^ s

  let map_field ~search_mode s =
    if search_mode = `Not_Exact_Prefix then s ^ "..." else s

  let first_name (query_params : Page.Advanced_search.Query_params.t) =
    map_field ~search_mode:query_params.first_name_search_mode
      (Option.value ~default:"" query_params.first_name)

  let surname (query_params : Page.Advanced_search.Query_params.t) =
    map_field ~search_mode:query_params.surname_search_mode
      (Option.value ~default:"" query_params.surname)

  let occupation (query_params : Page.Advanced_search.Query_params.t) =
    Option.value ~default:"" query_params.occupation

  let events ~(query_params : Page.Advanced_search.Query_params.t) conf =
    let event_string = event_search ~query_params conf in
    (* Adding the place and date at the end for the OR request. *)
    match query_params.event_search_mode with
    | `And -> event_string
    | `Or ->
        let place, date =
          match query_params.events with
          | [] -> (None, (None, None))
          | (_, event) :: _ -> (event.place, event.dates)
        in
        if
          Option.is_some place
          || Option.is_some (snd date)
          || Option.is_some (fst date)
        then
          get_place_date_request conf
            (Option.value ~default:"" place)
            date event_string
        else event_string

  let union (query_params : Page.Advanced_search.Query_params.t) =
    match query_params.married with
    | Some true -> 0
    | Some false -> 1
    | None -> 2

  let other_aliases (query_params : Page.Advanced_search.Query_params.t) =
    Option.value ~default:"" query_params.alias
end

(*
  Returns a description string for the current advanced search results in the correct language.
  e.g. "Search all Pierre, born in Paris, died in Paris"
*)
let searching_fields ~(query_params : Page.Advanced_search.Query_params.t) conf
    base =
  let search = "" in
  let map_field search_mode = SearchingFields.map_field ~search_mode in
  let search =
    SearchingFields.string_field
      ~map_field:(map_field query_params.first_name_search_mode)
      (Option.value ~default:"" query_params.first_name)
      search
  in
  let search =
    SearchingFields.string_field
      ~map_field:(map_field query_params.surname_search_mode)
      (Option.value ~default:"" query_params.surname)
      search
  in
  let search = SearchingFields.sosa_field ~query_params conf base search in
  let event_search = SearchingFields.event_search ~query_params conf in
  let search =
    if search = "" then event_search
    else if event_search = "" then search
    else search ^ ", " ^ event_search
  in
  (* Adding the place and date at the end for the OR request. *)
  let search =
    match query_params.event_search_mode with
    | `And -> search
    | `Or ->
        let place, date =
          match query_params.events with
          | [] -> (None, (None, None))
          | (_, event) :: _ -> (event.place, event.dates)
        in
        if
          Option.is_some place
          || Option.is_some (snd date)
          || Option.is_some (fst date)
        then
          SearchingFields.get_place_date_request conf
            (Option.value ~default:"" place)
            date search
        else search
  in
  let search =
    let marriage_place =
      Page.Advanced_search.Query_params.get_event_place ~event_kind:`Marriage
        query_params
    in
    if
      not
        (SearchingFields.test_string (Option.value ~default:"" marriage_place)
        || SearchingFields.test_date
             (Page.Advanced_search.Query_params.get_event_dates
                ~event_kind:`Marriage query_params))
    then
      let sep = if search <> "" then ", " else "" in
      match query_params.married with
      | Some true -> search ^ sep ^ Util.transl conf "having a family"
      | Some false -> search ^ sep ^ Util.transl conf "having no family"
      | None -> search
    else search
  in
  let sep = if search <> "" then "," else "" in
  Adef.safe
  @@ SearchingFields.string_field
       (Option.value ~default:"" query_params.occupation)
       (search ^ sep)

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

let prefix_matching_first_name_aliases ~first_name =
  filter_alias ~name:first_name ~matching:is_subset_pfx

let matching_surname_aliases ~surname =
  filter_alias ~name:surname ~matching:Ext_list.is_subset

let exact_matching_surname_aliases ~surname =
  filter_alias ~name:surname ~matching:Ext_list.elements_cmp

let prefix_matching_surname_aliases ~surname =
  filter_alias ~name:surname ~matching:is_subset_pfx

let matching_alias_public_name_qualifiers ~string =
  filter_alias ~name:string ~matching:Ext_list.elements_cmp

let force_exact_search_by_name conf =
  let is_exact_search_by_name_mode_key key =
    List.mem key [ "exact_first_name"; "exact_surname" ]
  in
  let module Config_env = Set.Make (struct
    type t = string * Adef.encoded_string

    let compare = compare
  end) in
  let exact_search_by_name_parameters =
    let on = Mutil.encode "on" in
    Config_env.of_list [ ("exact_first_name", on); ("exact_surname", on) ]
  in
  let make_env env =
    Config_env.elements exact_search_by_name_parameters
    @ List.filter
        (fun (key, _) -> not @@ is_exact_search_by_name_mode_key key)
        env
  in
  { conf with Config.env = make_env conf.Config.env }
