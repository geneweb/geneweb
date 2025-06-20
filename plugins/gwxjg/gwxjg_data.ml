module Ezgw = Gwxjg_ezgw
module Lexicon_parser = Gwxjg_lexicon_parser
module Sosa = Geneweb_sosa
module Db = Geneweb_db
open Geneweb
open Jingoo
open Jg_types
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let person_ht = Hashtbl.create 32
let mk_opt fn = function None -> Tnull | Some x -> fn x
let safe (x : Adef.safe_string) = Tsafe (x :> string)
let encoded (x : Adef.encoded_string) = Tsafe (x :> string)
let escaped (x : Adef.escaped_string) = Tsafe (x :> string)

let unbox_string = function
  | Tsafe s | Tstr s -> s
  | x -> Jg_types.failwith_type_error_1 "unbox_string" x

let mk_source_rs conf base str = (Tstr str, safe (Notes.source conf base str))

let mk_note_rs conf base env str =
  (Tstr str, safe (Notes.note conf base env str))

let mk_person_note_rs conf base p str =
  (Tstr str, safe (Notes.person_note conf base p str))

let mk_place conf str = (Tstr str, escaped (Util.string_of_place conf str))

let rec date_compare_aux date1 date2 =
  let y1 = field date1 "year" in
  let y2 = field date2 "year" in
  match Jg_runtime.jg_compare y1 y2 with
  | Tint 0 -> compare_month date1 date2
  | x -> x

and compare_month date1 date2 =
  match (field date1 "month", field date2 "month") with
  | Tint 0, Tint 0 -> cmp_prec date1 date2
  | Tint 0, Tint _ ->
      if field date1 "prec" = Tsafe "after" then Tint 1
      else cmp_prec date1 date2
  | Tint _, Tint 0 ->
      if field date2 "prec" = Tsafe "after" then Tint (-1)
      else cmp_prec date1 date2
  | m1, m2 -> (
      match Jg_runtime.jg_compare m1 m2 with
      | Tint 0 -> compare_day date1 date2
      | x -> x)

and compare_day date1 date2 =
  match (field date1 "day", field date2 "day") with
  | Tint 0, Tint 0 -> cmp_prec date1 date2
  | Tint 0, Tint _ ->
      if field date1 "prec" = Tsafe "after" then Tint 1
      else cmp_prec date1 date2
  | Tint _, Tint 0 ->
      if field date2 "prec" = Tsafe "after" then Tint (-1)
      else cmp_prec date1 date2
  | d1, d2 -> (
      match Jg_runtime.jg_compare d1 d2 with
      | Tint 0 -> cmp_prec date1 date2
      | x -> x)

and cmp_prec d1 d2 =
  match (field d1 "prec", field d2 "prec") with
  | Tsafe ("sure" | "about" | "maybe"), Tsafe ("sure" | "about" | "maybe") ->
      Tint 0
  | Tsafe "after", Tsafe "after" | Tsafe "before", Tsafe "before" -> Tint 0
  | Tsafe "oryear", Tsafe "oryear" | Tsafe "yearint", Tsafe "yearint" ->
      date_compare_aux (field d1 "d2") (field d2 "d2")
  | _, Tsafe "after" | Tsafe "before", _ -> Tint (-1)
  | Tsafe "after", _ | _, Tsafe "before" -> Tint 1
  | _ -> Tint 0

and field = Jg_runtime.jg_obj_lookup

let rec mk_family (conf : Config.config) base
    ((_, fam, (ifath, imoth, ispouse), _) as fcd) =
  let module E = Ezgw.Family in
  let f = E.father fcd in
  let m = E.mother fcd in
  let father = lazy_get_n_mk_person conf base f in
  let mother = lazy_get_n_mk_person conf base m in
  let spouse =
    if ifath = ispouse then father
    else if imoth = ispouse then mother
    else Tnull
  in
  let children =
    Tarray (Array.map (lazy_get_n_mk_person conf base) (E.children fcd))
  in
  let events' = E.events fcd in
  let events = lazy_list (mk_event conf base) events' in
  let relation =
    match Driver.get_relation fam with
    | Def.Married | NoSexesCheckMarried ->
        find_event conf base (Event.Fevent Def.Efam_Marriage) events'
    | NotMarried | NoSexesCheckNotMarried ->
        find_event conf base (Event.Fevent Def.Efam_NoMarriage) events'
    | Engaged -> find_event conf base (Event.Fevent Def.Efam_Engage) events'
    | NoMention ->
        find_event conf base (Event.Fevent Def.Efam_NoMention) events'
    | MarriageBann ->
        find_event conf base (Event.Fevent Def.Efam_MarriageBann) events'
    | MarriageContract ->
        find_event conf base (Event.Fevent Def.Efam_MarriageContract) events'
    | MarriageLicense ->
        find_event conf base (Event.Fevent Def.Efam_MarriageLicense) events'
    | Pacs -> find_event conf base (Event.Fevent Def.Efam_PACS) events'
    | Residence ->
        find_event conf base (Event.Fevent Def.Efam_Residence) events'
  in
  let separation =
    match Driver.get_divorce fam with
    | Def.Divorced _ ->
        find_event conf base (Event.Fevent Def.Efam_Divorce) events'
    | Def.Separated _ ->
        find_event conf base (Event.Fevent Def.Efam_Separated) events'
    | Def.Separated_old ->
        find_event conf base (Event.Fevent Def.Efam_Separated) events'
    | NotDivorced -> Tnull
    | NotSeparated -> Tnull
  in
  let ifam = Tstr (E.ifam fcd) in
  let origin_file = Tstr (E.origin_file conf base fcd) in
  let note_raw, note = mk_note_rs conf base [] (E.note conf base fcd) in
  let source_raw, source = mk_source_rs conf base (E.sources base fcd) in
  Tpat
    (function
    | "children" -> children
    | "father" -> father
    | "events" -> events
    | "ifam" -> ifam
    | "mother" -> mother
    | "note" -> note
    | "note_raw" -> note_raw
    | "origin_file" -> origin_file
    | "relation" -> relation
    | "separation" -> separation
    | "spouse" -> spouse
    | "source" -> source
    | "source_raw" -> source_raw
    | _ -> raise Not_found)

and get_n_mk_family conf base ?(origin = Driver.Iper.dummy) ifam cpl =
  let ifath = Driver.get_father cpl in
  let imoth = Driver.get_mother cpl in
  let cpl =
    ( ifath,
      imoth,
      if ifath = origin then imoth else if imoth = origin then ifath else origin
    )
  in
  let m_auth =
    Util.authorized_age conf base (Driver.poi base ifath)
    && Util.authorized_age conf base (Driver.poi base imoth)
  in
  mk_family conf base (ifam, Driver.foi base ifam, cpl, m_auth)

and date_compare = func_arg2_no_kw date_compare_aux

and date_eq =
  func_arg2_no_kw (fun d1 d2 -> Tbool (date_compare_aux d1 d2 = Tint 0))

and dtext_eq =
  func_arg2_no_kw @@ fun d1 d2 ->
  Tbool
    (Jg_runtime.jg_obj_lookup d1 "__str__"
    = Jg_runtime.jg_obj_lookup d2 "__str__")

and mk_dmy { Def.day; month; year; delta; prec } =
  let day = Tint day in
  let month = Tint month in
  let year = Tint year in
  let delta = Tint delta in
  let prec = to_prec prec in
  Tpat
    (function
    | "day" -> day
    | "month" -> month
    | "year" -> year
    | "delta" -> delta
    | "prec" -> prec
    | _ -> raise Not_found)

and mk_date = function
  | Def.Dtext s ->
      Tpat
        (function
        | "__str__" -> Tstr s
        | "__compare__" -> func_arg2_no_kw (fun _ _ -> Tint 0)
        | "__eq__" -> dtext_eq
        | "__Dtext__" -> Tbool true
        | _ -> raise Not_found)
  | Dgreg (d, c) ->
      let year = Tint d.Def.year in
      let month = Tint d.Def.month in
      let day = Tint d.Def.day in
      let prec = to_prec d.Def.prec in
      let d2 =
        match d.Def.prec with
        | OrYear d2 | YearInt d2 ->
            mk_dmy
              {
                Def.day = d2.Def.day2;
                month = d2.Def.month2;
                year = d2.Def.year2;
                prec = Def.Sure;
                delta = 0;
              }
        | _ -> Tnull
      in
      let calendar =
        match c with
        | Dgregorian -> Tsafe "Dgregorian"
        | Djulian -> Tsafe "Djulian"
        | Dfrench -> Tsafe "Dfrench"
        | Dhebrew -> Tsafe "Dhebrew"
      in
      Tpat
        (function
        | "calendar" -> calendar
        | "d2" -> d2
        | "day" -> day
        | "month" -> month
        | "prec" -> prec
        | "year" -> year
        | "__compare__" -> date_compare
        | "__eq__" -> date_eq
        | _ -> raise Not_found)

and to_dmy d =
  let int s = match Jg_runtime.jg_obj_lookup d s with Tint i -> i | _ -> 0 in
  {
    Def.day = int "day";
    month = int "month";
    year = int "year";
    prec = of_prec d;
    delta = 0;
  }

and to_dmy2 d =
  let int s = match Jg_runtime.jg_obj_lookup d s with Tint i -> i | _ -> 0 in
  { Def.day2 = int "day"; month2 = int "month"; year2 = int "year"; delta2 = 0 }

and to_prec = function
  | Def.Sure -> Tsafe "sure"
  | About -> Tsafe "about"
  | Maybe -> Tsafe "maybe"
  | Before -> Tsafe "before"
  | After -> Tsafe "after"
  | OrYear _ -> Tsafe "oryear"
  | YearInt _ -> Tsafe "yearint"

and of_prec d =
  match Jg_runtime.jg_obj_lookup d "prec" with
  | Tsafe "sure" -> Def.Sure
  | Tsafe "about" -> About
  | Tsafe "maybe" -> Maybe
  | Tsafe "before" -> Before
  | Tsafe "after" -> After
  | Tsafe "oryear" -> OrYear (to_dmy2 @@ Jg_runtime.jg_obj_lookup d "d2")
  | Tsafe "yearint" -> YearInt (to_dmy2 @@ Jg_runtime.jg_obj_lookup d "d2")
  | _ -> assert false

and to_gregorian_aux calendar d =
  let d = to_dmy d in
  match calendar with
  | "Dgregorian" -> d
  | "Djulian" -> Calendar.gregorian_of_julian d
  | "Dfrench" -> Calendar.gregorian_of_french d
  | "Dhebrew" -> Calendar.gregorian_of_hebrew d
  | _ -> assert false

and of_calendar d =
  match Jg_runtime.jg_obj_lookup d "calendar" with
  | Tsafe "Dgregorian" -> Def.Dgregorian
  | Tsafe "Djulian" -> Def.Djulian
  | Tsafe "Dfrench" -> Def.Dfrench
  | Tsafe "Dhebrew" -> Def.Dhebrew
  | _ -> assert false

and module_DATE conf =
  let now =
    Tvolatile
      (fun () ->
        let now = Unix.gmtime @@ Unix.time () in
        let day = Tint now.tm_mday in
        let month = Tint (now.tm_mon + 1) in
        let year = Tint (now.tm_year + 1900) in
        Tpat
          (function
          | "day" -> day
          | "month" -> month
          | "year" -> year
          | "prec" -> Tsafe "sure"
          | _ -> raise Not_found))
  in
  let death_symbol = Tsafe (DateDisplay.death_symbol conf) in
  let string_of_date_aux fn =
    func_arg1_no_kw @@ fun d ->
    try safe (Def.Dgreg (to_dmy d, of_calendar d) |> fn conf)
    with e ->
      if Jg_runtime.jg_obj_lookup d "__Dtext__" = Tbool true then
        Jg_runtime.jg_obj_lookup d "__str__"
      else raise e
  in
  let string_of_date = string_of_date_aux DateDisplay.string_of_date in
  let string_of_ondate =
    string_of_date_aux (DateDisplay.string_of_ondate ~link:false)
  in
  let code_french_year =
    func_arg1_no_kw (fun i ->
        Tstr (DateDisplay.code_french_year conf (unbox_int i)))
  in
  let string_of_age =
    func_arg1_no_kw (fun d -> safe (DateDisplay.string_of_age conf (to_dmy d)))
  in
  let sub =
    func_arg2_no_kw (fun d1 d2 ->
        mk_dmy @@ Date.time_elapsed (to_dmy d2) (to_dmy d1))
  in
  let calendar =
    func_arg2_no_kw (fun dst d ->
        let convert fn = mk_dmy @@ fn @@ to_dmy d in
        match unbox_string @@ dst with
        | "Dgregorian" -> convert (fun x -> x)
        | "Djulian" -> convert Calendar.julian_of_gregorian
        | "Dfrench" -> convert Calendar.french_of_gregorian
        | "Dhebrew" -> convert Calendar.hebrew_of_gregorian
        | s -> failwith @@ "Unknown calendar: " ^ s)
  in
  Tpat
    (function
    | "calendar" -> calendar
    | "compare" -> date_compare
    | "death_symbol" -> death_symbol
    | "code_french_year" -> code_french_year
    | "eq" -> date_eq
    | "now" -> now
    | "string_of_age" -> string_of_age
    | "string_of_date" -> string_of_date
    | "string_of_ondate" -> string_of_ondate
    | "sub" -> sub
    | _ -> raise Not_found)

and lazy_list : 'a. ('a -> tvalue) -> 'a list -> tvalue =
 fun fn -> function [] -> Tlist [] | l -> Tlazy (lazy (Tlist (List.map fn l)))

and lazy_get_n_mk_person conf base i =
  let lp = lazy (get_n_mk_person conf base i) in
  let iper = Tstr (Driver.Iper.to_string i) in
  Tpat (function "iper" -> iper | s -> unbox_pat (Lazy.force lp) s)

and ppget conf base p =
  let open Config in
  let open Def in
  if not (Util.authorized_age conf base p) then
    if conf.use_restrict then
      Driver.get_iper p |> Driver.empty_person base
      |> unsafe_mk_person conf base
    else if conf.hide_names || Driver.get_access p = Private then
      let lazy_p =
        lazy (unbox_pat @@ unsafe_mk_semi_public_person conf base p)
      in
      Tpat
        (function
        | "first_name" | "surname" -> Tsafe "x"
        | "first_name_aliases" | "surname_aliases" -> Tlist []
        | x -> (Lazy.force lazy_p) x)
    else unsafe_mk_semi_public_person conf base p
  else unsafe_mk_person conf base p

and pget conf base ip =
  if ip = Driver.Iper.dummy then
    unsafe_mk_person conf base (Driver.empty_person base ip)
  else ppget conf base (Driver.poi base ip)

and get_n_mk_person conf base (i : Driver.iper) =
  try Hashtbl.find person_ht i
  with Not_found ->
    let p = pget conf base i in
    Hashtbl.add person_ht i p;
    p

and mk_rparent_aux kind conf base acc =
  let mk_rel i t s =
    let iper = Tstr (Driver.Iper.to_string i) in
    let kind = kind t in
    let sources = Tstr (Driver.sou base s) in
    let lp = lazy (get_n_mk_person conf base i) in
    Tpat
      (function
      | "source" -> sources
      | "kind" -> kind
      | "iper" -> iper
      | s -> unbox_pat (Lazy.force lp) s)
  in
  function
  | { Def.r_fath = None; r_moth = Some i; r_type; r_sources }
  | { r_fath = Some i; r_moth = None; r_type; r_sources } ->
      mk_rel i r_type r_sources :: acc
  | { r_fath = Some i1; r_moth = Some i2; r_type; r_sources } ->
      mk_rel i1 r_type r_sources :: mk_rel i2 r_type r_sources :: acc
  | _ -> Tnull :: acc

and mk_rparent conf base acc =
  mk_rparent_aux
    (function
      | Def.Adoption -> Tsafe "ADOPTIVE_PARENT"
      | Def.CandidateParent -> Tsafe "CANDIDATE_PARENT"
      | Def.FosterParent -> Tsafe "FOSTER_PARENT"
      | Def.GodParent -> Tsafe "GODPARENT"
      | Def.Recognition -> Tsafe "RECOGNIZING_PARENT")
    conf base acc

and mk_rchild conf base acc =
  mk_rparent_aux
    (function
      | Def.Adoption -> Tsafe "ADOPTIVE_CHILD"
      | Def.CandidateParent -> Tsafe "CANDIDATE_CHILD"
      | Def.FosterParent -> Tsafe "FOSTER_CHILD"
      | Def.GodParent -> Tsafe "GODCHILD"
      | Def.Recognition -> Tsafe "RECOGNIZED_CHILD")
    conf base acc

and mk_witness_kind = function
  | Def.Witness -> Tsafe "WITNESS"
  | Def.Witness_GodParent -> Tsafe "WITNESS_GODPARENT"
  | Def.Witness_CivilOfficer -> Tsafe "WITNESS_CIVILOFFICER"
  | Def.Witness_ReligiousOfficer -> Tsafe "WITNESS_RELIGIOUSOFFICER"
  | Def.Witness_Informant -> Tsafe "WITNESS_INFORMANT"
  | Def.Witness_Attending -> Tsafe "WITNESS_ATTENDING"
  | Def.Witness_Mentioned -> Tsafe "WITNESS_MENTIONED"
  | Def.Witness_Other -> Tsafe "WITNESS_OTHER"

and mk_event conf base d =
  let module E = Ezgw.Event in
  let date = match E.date d with Some d -> mk_date d | None -> Tnull in
  let name = safe (E.name conf base d) in
  let spouse =
    match E.spouse_opt d with
    | None -> Tnull
    | Some i -> lazy_get_n_mk_person conf base i
  in
  let kind = Tsafe (E.kind d) in
  let witnesses =
    match E.witnesses d with
    | [||] -> Tarray [||]
    | w ->
        let lw =
          lazy (Array.map (fun (i, _) -> get_n_mk_person conf base i) w)
        in
        (* We may want to filter on [ip] or [k] before really accessing the person entity *)
        Tarray
          (Array.mapi
             (fun i (ip, k) ->
               let kind = mk_witness_kind k in
               let iper = Tstr (Driver.Iper.to_string ip) in
               Tpat
                 (function
                 | "kind" -> kind
                 | "iper" -> iper
                 | s -> unbox_pat (Lazy.force lw).(i) @@ s))
             w)
  in
  let place_raw, place = mk_place conf (E.place base d) in
  let source_raw, source = mk_source_rs conf base (E.src base d) in
  let note_raw, note = mk_note_rs conf base [] (E.note conf base d) in
  Tpat
    (function
    | "date" -> date
    | "kind" -> kind
    | "name" -> name
    | "note" -> note
    | "note_raw" -> note_raw
    | "place" -> place
    | "place_raw" -> place_raw
    | "spouse" -> spouse
    | "source" -> source
    | "source_raw" -> source_raw
    | "witnesses" -> witnesses
    | _ -> raise Not_found)

and mk_title conf base t =
  let ident = Tstr (Driver.sou base t.Def.t_ident) in
  let name =
    match t.t_name with
    | Tmain -> Tstr ""
    | Tname s -> Tstr (Driver.sou base s)
    | Tnone -> Tnull
  in
  let place_raw, place = mk_place conf (Driver.sou base t.t_place) in
  let date_start = mk_opt mk_date (Date.od_of_cdate t.t_date_start) in
  let date_end = mk_opt mk_date (Date.od_of_cdate t.t_date_end) in
  let nth = Tint t.t_nth in
  Tpat
    (function
    | "ident" -> ident
    | "name" -> name
    | "place" -> place
    | "place_raw" -> place_raw
    | "date_start" -> date_start
    | "date_end" -> date_end
    | "nth" -> nth
    | _ -> raise Not_found)

and mk_ancestors conf base (p : Driver.person) =
  let parents =
    match Driver.get_parents p with
    | Some ifam -> Some (lazy (Driver.foi base ifam))
    | None -> None
  in
  let mk_parent fn =
    match parents with
    | Some f -> Tlazy (lazy (get_n_mk_person conf base (fn @@ Lazy.force f)))
    | None -> Tnull
  in
  let parents =
    match parents with
    | None -> Tnull
    | Some f ->
        Tlazy
          (lazy
            (let cpl = Lazy.force f in
             let ifam = Driver.get_ifam cpl in
             let ifath = Driver.get_father cpl in
             let imoth = Driver.get_mother cpl in
             let cpl = (ifath, imoth, Driver.Iper.dummy) in
             let m_auth =
               Util.authorized_age conf base (Driver.poi base ifath)
               && Util.authorized_age conf base (Driver.poi base imoth)
             in
             mk_family conf base (ifam, Driver.foi base ifam, cpl, m_auth)))
  in
  let father = mk_parent Driver.get_father in
  let mother = mk_parent Driver.get_mother in
  (parents, father, mother)

and mk_rparents conf base (p : Driver.person) =
  let mkr p r =
    if Driver.get_sex p = Def.Female then
      { r with Def.r_fath = None; r_moth = Some (Driver.get_iper p) }
    else { r with Def.r_fath = Some (Driver.get_iper p); r_moth = None }
  in
  match Driver.get_rparents p with
  | [] -> Tlist []
  | r ->
      box_list
      @@ List.fold_left (mk_rparent conf base)
           (List.fold_left
              (fun acc i ->
                let p = Util.pget conf base i in
                List.fold_left
                  (fun acc r -> mk_rchild conf base acc @@ mkr p r)
                  acc (Driver.get_rparents p))
              []
           @@ Driver.get_related p)
           r

and mk_families_spouses iper conf base (p : Driver.person) =
  let lazy_families =
    lazy (Array.map (fun i -> (i, Driver.foi base i)) @@ Driver.get_family p)
  in
  let families =
    Tlazy
      (lazy
        (Tarray
           (Array.map
              (fun (ifam, cpl) ->
                get_n_mk_family conf base ~origin:iper ifam cpl)
              (Lazy.force lazy_families))))
  in
  let spouses =
    Tlazy
      (lazy
        (Tarray
           (Array.map
              (fun (_, c) ->
                let f = Driver.get_father c in
                get_n_mk_person conf base
                  (if f = iper then Driver.get_mother c else f))
              (Lazy.force lazy_families))))
  in
  (families, spouses)

and mk_str_lst base istrs =
  Tlist (List.map (fun i -> Tstr (Driver.sou base i)) istrs)

and unsafe_mk_semi_public_person conf base (p : Driver.person) =
  let iper' = Driver.get_iper p in
  let module E = Ezgw.Person in
  let access = escaped (Util.acces conf base p) in
  let parents, father, mother = mk_ancestors conf base p in
  let families, spouses = mk_families_spouses iper' conf base p in
  let first_name = Tstr (E.first_name base p) in
  let first_name_aliases = mk_str_lst base (Driver.get_first_names_aliases p) in
  let children = lazy_list (get_n_mk_person conf base) (E.children base p) in
  let iper = Tstr (Driver.Iper.to_string iper') in
  let related = mk_rparents conf base p in
  let siblings_aux fn = lazy_list (get_n_mk_person conf base) (fn base p) in
  let siblings = siblings_aux E.siblings in
  let half_siblings = siblings_aux E.half_siblings in
  let sex = Tint (E.sex p) in
  let surname = Tstr (E.surname base p) in
  let surname_aliases = mk_str_lst base (Driver.get_surnames_aliases p) in
  let events = Tlist [] in
  Tpat
    (function
    | "access" -> access
    | "children" -> children
    | "events" -> events
    | "families" -> families
    | "father" -> father
    | "first_name" -> first_name
    | "first_name_aliases" -> first_name_aliases
    | "half_siblings" -> half_siblings
    | "iper" -> iper
    | "mother" -> mother
    | "parents" -> parents
    | "related" -> related
    | "sex" -> sex
    | "siblings" -> siblings
    | "spouses" -> spouses
    | "surname" -> surname
    | "surname_aliases" -> surname_aliases
    | _ -> raise Not_found)

and get_sosa_person =
  let loaded = ref false in
  fun conf base p ->
    if not !loaded then (
      SosaCache.build_sosa_ht conf base;
      loaded := true);
    let sosa = SosaCache.get_sosa_person p in
    if sosa = Sosa.zero then Tnull else Tstr (Sosa.to_string sosa)

and find_event conf base x events =
  match List.find_opt (fun (x', _, _, _, _, _, _) -> x' = x) events with
  | Some e -> mk_event conf base e
  | None -> Tnull

and find_events conf base x events =
  match List.find_opt (fun (x', _, _, _, _, _, _) -> List.mem x' x) events with
  | Some e -> mk_event conf base e
  | None -> Tnull

and unsafe_mk_person conf base (p : Driver.person) =
  let module E = Ezgw.Person in
  let iper' = Driver.get_iper p in
  let access = escaped (Util.acces conf base p) in
  let parents, father, mother = mk_ancestors conf base p in
  let families, spouses = mk_families_spouses iper' conf base p in
  let aliases = mk_str_lst base (Driver.get_aliases p) in
  let children = lazy_list (get_n_mk_person conf base) (E.children base p) in
  let consanguinity = Tfloat (E.consanguinity p) in
  let events' = E.events conf base p in
  let events = lazy_list (mk_event conf base) events' in
  let birth = find_event conf base (Event.Pevent Epers_Birth) events' in
  let baptism = find_event conf base (Event.Pevent Epers_Baptism) events' in
  let death =
    let wrap s = Tpat (function "reason" -> Tsafe s | _ -> raise Not_found) in
    match Driver.get_death p with
    | Def.NotDead -> Tnull
    | Death (r, _cd) ->
        let reason =
          match r with
          | Def.Killed -> Tsafe "Killed"
          | Murdered -> Tsafe "Murdered"
          | Executed -> Tsafe "Executed"
          | Disappeared -> Tsafe "Disappeared"
          | Unspecified -> Tsafe "Unspecified"
        in
        let e = find_event conf base (Event.Pevent Epers_Death) events' in
        Tpat (function "reason" -> reason | s -> Jg_runtime.jg_obj_lookup e s)
    | DeadYoung -> wrap "DeadYoung"
    | DeadDontKnowWhen -> wrap "DeadDontKnowWhen"
    | DontKnowIfDead -> wrap "DontKnowIfDead"
    | OfCourseDead -> wrap "OfCourseDead"
  in
  let burial =
    find_events conf base
      [ Event.Pevent Epers_Burial; Event.Pevent Epers_Cremation ]
      events'
  in
  let first_name = Tstr (E.first_name base p) in
  let first_name_aliases = mk_str_lst base (Driver.get_first_names_aliases p) in
  let image =
    Tstr
      (Image.get_portrait conf base p
      |> Option.fold ~none:"" ~some:Image.src_to_string)
  in
  let iper = Tstr (Driver.Iper.to_string iper') in
  let linked_page =
    Tlazy
      (lazy
        (let db = Driver.read_nldb base in
         let db = Notes.merge_possible_aliases conf db in
         let key =
           let fn = Name.lower (Driver.sou base (Driver.get_first_name p)) in
           let sn = Name.lower (Driver.sou base (Driver.get_surname p)) in
           (fn, sn, Driver.get_occ p)
         in
         if
           List.exists
             (fun (pg, (_, il)) ->
               (match pg with
               | Def.NLDB.PgInd ip ->
                   Util.pget conf base ip |> Util.authorized_age conf base
               | Def.NLDB.PgFam ifam ->
                   Driver.foi base ifam |> Driver.get_father
                   |> Util.pget conf base
                   |> Util.authorized_age conf base
               | Def.NLDB.PgNotes | Def.NLDB.PgMisc _ | Def.NLDB.PgWizard _ ->
                   true)
               && List.exists (fun (k, _) -> k = key) il)
             db
         then
           Tpat
             (fun s ->
               safe
                 (List.fold_left
                    (Perso.linked_page_text conf base p s key)
                    (Adef.safe "") db))
         else Tnull))
  in
  let titles = lazy_list (mk_title conf base) (Driver.get_titles p) in
  let _, note = mk_person_note_rs conf base p (E.note conf base p) in
  let occ = Tint (Driver.get_occ p) in
  let occupation_raw, occupation =
    mk_source_rs conf base (Driver.sou base @@ Driver.get_occupation p)
  in
  let public_name = Tstr (Driver.sou base @@ Driver.get_public_name p) in
  let qualifiers = mk_str_lst base (Driver.get_qualifiers p) in
  let related = mk_rparents conf base p in
  let relations = lazy_list (get_n_mk_person conf base) (E.relations p) in
  let sex = Tint (E.sex p) in
  let siblings_aux fn = lazy_list (get_n_mk_person conf base) (fn base p) in
  let siblings = siblings_aux E.siblings in
  let half_siblings = siblings_aux E.half_siblings in
  let source_raw, source =
    mk_source_rs conf base (Driver.sou base @@ Driver.get_psources p)
  in
  let surname = Tstr (E.surname base p) in
  let surname_aliases = mk_str_lst base (Driver.get_surnames_aliases p) in
  let sosa = box_lazy @@ lazy (get_sosa_person conf base p) in
  Tpat
    (function
    | "access" -> access
    | "aliases" -> aliases
    | "baptism" -> baptism
    | "birth" -> birth
    | "burial" -> burial
    | "children" -> children
    | "consanguinity" -> consanguinity
    | "death" -> death
    | "events" -> events
    | "families" -> families
    | "father" -> father
    | "first_name" -> first_name
    | "first_name_aliases" -> first_name_aliases
    | "half_siblings" -> half_siblings
    | "image" -> image
    | "iper" -> iper
    | "linked_page" -> linked_page
    | "mother" -> mother
    | "note" -> note
    | "occ" -> occ
    | "occupation" -> occupation
    | "occupation_raw" -> occupation_raw
    | "parents" -> parents
    | "public_name" -> public_name
    | "qualifiers" -> qualifiers
    | "relations" -> relations
    | "related" -> related
    | "sex" -> sex
    | "siblings" -> siblings
    | "sosa" -> sosa
    | "source" -> source
    | "source_raw" -> source_raw
    | "spouses" -> spouses
    | "surname" -> surname
    | "surname_aliases" -> surname_aliases
    | "titles" -> titles
    | _ -> raise Not_found)

and mk_fevent ?spouse conf base e =
  mk_event conf base
    ( Fevent e.Def.efam_name,
      e.efam_date,
      e.efam_place,
      e.efam_note,
      e.efam_src,
      e.efam_witnesses,
      spouse )

and mk_pevent conf base e =
  mk_event conf base
    ( Pevent e.Def.epers_name,
      e.epers_date,
      e.epers_place,
      e.epers_note,
      e.epers_src,
      e.epers_witnesses,
      None )

(* take optionnal p parameter for spouse things? *)
and mk_warning conf base =
  let get_fam ifam =
    let cpl = Driver.foi base ifam in
    let ifath = Driver.get_father cpl in
    let imoth = Driver.get_mother cpl in
    (* spouse if not used so it should be okay *)
    mk_family conf base (ifam, cpl, (ifath, imoth, imoth), true)
  in
  let array_of_list_map : 'a 'b. ('a -> 'b) -> 'a list -> 'b array =
   fun fn l ->
    if l = [] then [||]
    else
      let a = Array.make (List.length l) (fn @@ List.hd l) in
      (* FIXME *)
      List.iteri (fun i x -> a.(i) <- fn x) l;
      a
  in
  function
  | Def.BigAgeBetweenSpouses (f, m, a) ->
      Tset
        [
          Tsafe "BigAgeBetweenSpouses";
          unsafe_mk_person conf base f;
          unsafe_mk_person conf base m;
          mk_date (Dgreg (a, Dgregorian));
        ]
  | BirthAfterDeath p ->
      Tset [ Tsafe "BirthAfterDeath"; unsafe_mk_person conf base p ]
  | IncoherentSex (p, i1, i2) ->
      Tset
        [
          Tsafe "BirthAfterDeath";
          unsafe_mk_person conf base p;
          Tint i1;
          Tint i2;
        ]
  | ChangedOrderOfChildren (ifam, _descend, before, after) ->
      let bef_d, aft_d = Difference.f before after in
      Tset
        [
          Tsafe "ChangedOrderOfChildren";
          get_fam ifam;
          Tarray (Array.map (get_n_mk_person conf base) before);
          Tarray (Array.map (get_n_mk_person conf base) after);
          Tarray (Array.map box_bool bef_d);
          Tarray (Array.map box_bool aft_d);
        ]
  | ChangedOrderOfMarriages (p, before, after) ->
      let bef_d, aft_d = Difference.f before after in
      Tset
        [
          Tsafe "ChangedOrderOfMarriages";
          unsafe_mk_person conf base p;
          Tarray (Array.map get_fam before);
          Tarray (Array.map get_fam after);
          Tarray (Array.map box_bool bef_d);
          Tarray (Array.map box_bool aft_d);
        ]
  | ChangedOrderOfFamilyEvents (_ifam, before, after) ->
      let before = array_of_list_map (mk_fevent conf base) before in
      let after = array_of_list_map (mk_fevent conf base) after in
      let bef_d, aft_d = Difference.f before after in
      Tset
        [
          Tsafe "ChangedOrderOfFamilyEvents";
          Tarray before;
          Tarray after;
          Tarray (Array.map box_bool bef_d);
          Tarray (Array.map box_bool aft_d);
        ]
  | ChangedOrderOfPersonEvents (_p, before, after) ->
      let before = array_of_list_map (mk_pevent conf base) before in
      let after = array_of_list_map (mk_pevent conf base) after in
      let bef_d, aft_d = Difference.f before after in
      Tset
        [
          Tsafe "ChangedOrderOfPersonEvents";
          Tarray before;
          Tarray after;
          Tarray (Array.map box_bool bef_d);
          Tarray (Array.map box_bool aft_d);
        ]
  | ChildrenNotInOrder (ifam, _descend, elder, x) ->
      Tset
        [
          Tsafe "ChildrenNotInOrder";
          get_fam ifam;
          unsafe_mk_person conf base elder;
          unsafe_mk_person conf base x;
        ]
  | CloseChildren (ifam, c1, c2) ->
      Tset
        [
          Tsafe "CloseChildren";
          get_fam ifam;
          unsafe_mk_person conf base c1;
          unsafe_mk_person conf base c2;
        ]
  | DistantChildren (ifam, c1, c2) ->
      Tset
        [
          Tsafe "DistantChildren";
          get_fam ifam;
          unsafe_mk_person conf base c1;
          unsafe_mk_person conf base c2;
        ]
  | DeadOld (p, a) ->
      Tset
        [
          Tsafe "DeadOld";
          unsafe_mk_person conf base p;
          mk_date (Dgreg (a, Dgregorian));
        ]
  | DeadTooEarlyToBeFather (father, child) ->
      Tset
        [
          Tsafe "DeadTooEarlyToBeFather";
          unsafe_mk_person conf base father;
          unsafe_mk_person conf base child;
        ]
  | FEventOrder (p, e1, e2) ->
      Tset
        [
          Tsafe "FEventOrder";
          unsafe_mk_person conf base p;
          mk_fevent conf base e1;
          mk_fevent conf base e2;
        ]
  | FWitnessEventAfterDeath (p, e, ifam) ->
      Tset
        [
          Tsafe "FWitnessEventAfterDeath";
          unsafe_mk_person conf base p;
          mk_fevent conf base e;
          get_fam ifam;
        ]
  | FWitnessEventBeforeBirth (p, e, ifam) ->
      Tset
        [
          Tsafe "FWitnessEventBeforeBirth";
          unsafe_mk_person conf base p;
          mk_fevent conf base e;
          get_fam ifam;
        ]
  | IncoherentAncestorDate (p1, p2) ->
      Tset
        [
          Tsafe "IncoherentAncestorDate";
          unsafe_mk_person conf base p1;
          unsafe_mk_person conf base p2;
        ]
  | MarriageDateAfterDeath p ->
      Tset [ Tsafe "MarriageDateAfterDeath"; unsafe_mk_person conf base p ]
  | MarriageDateBeforeBirth p ->
      Tset [ Tsafe "MarriageDateBeforeBirth"; unsafe_mk_person conf base p ]
  | MotherDeadBeforeChildBirth (p1, p2) ->
      Tset
        [
          Tsafe "MotherDeadBeforeChildBirth";
          unsafe_mk_person conf base p1;
          unsafe_mk_person conf base p2;
        ]
  | OldForMarriage (p, a, i) ->
      Tset
        [
          Tsafe "OldForMarriage";
          unsafe_mk_person conf base p;
          mk_date (Dgreg (a, Dgregorian));
          get_n_mk_family conf base i (Driver.foi base i);
        ]
  | ParentBornAfterChild (p1, p2) ->
      Tset
        [
          Tsafe "ParentBornAfterChild";
          unsafe_mk_person conf base p1;
          unsafe_mk_person conf base p2;
        ]
  | ParentTooOld (p, a, c) ->
      Tset
        [
          Tsafe "ParentTooOld";
          unsafe_mk_person conf base p;
          mk_date (Dgreg (a, Dgregorian));
          unsafe_mk_person conf base c;
        ]
  | ParentTooYoung (p, a, c) ->
      Tset
        [
          Tsafe "ParentTooYoung";
          unsafe_mk_person conf base p;
          mk_date (Dgreg (a, Dgregorian));
          unsafe_mk_person conf base c;
        ]
  | PEventOrder (p, e1, e2) ->
      Tset
        [
          Tsafe "PEventOrder";
          unsafe_mk_person conf base p;
          mk_pevent conf base e1;
          mk_pevent conf base e2;
        ]
  | PWitnessEventAfterDeath (p, e, origin) ->
      Tset
        [
          Tsafe "PWitnessEventAfterDeath";
          unsafe_mk_person conf base p;
          mk_pevent conf base e;
          unsafe_mk_person conf base origin;
        ]
  | PWitnessEventBeforeBirth (p, e, origin) ->
      Tset
        [
          Tsafe "PWitnessEventBeforeBirth";
          unsafe_mk_person conf base p;
          mk_pevent conf base e;
          unsafe_mk_person conf base origin;
        ]
  | TitleDatesError (p, t) ->
      Tset
        [
          Tsafe "TitleDatesError";
          unsafe_mk_person conf base p;
          mk_title conf base t;
        ]
  | UndefinedSex p ->
      Tset [ Tsafe "UndefinedSex"; unsafe_mk_person conf base p ]
  | YoungForMarriage (p, a, i) ->
      Tset
        [
          Tsafe "YoungForMarriage";
          unsafe_mk_person conf base p;
          mk_date (Dgreg (a, Dgregorian));
          get_fam i;
        ]
  | PossibleDuplicateFam (ifam1, ifam2) ->
      Tset [ Tsafe "PossibleDuplicateFam"; get_fam ifam1; get_fam ifam2 ]
  | PossibleDuplicateFamHomonymous (ifam1, ifam2, p) ->
      Tset
        [
          Tsafe "PossibleDuplicateFamHomonymous";
          get_fam ifam1;
          get_fam ifam2;
          unsafe_mk_person conf base p;
        ]

let module_OPT =
  let map =
    func_arg2_no_kw (fun fn -> function
      | Tnull -> Tnull
      | x -> (unbox_fun fn) x)
  in
  Tpat (function "map" -> map | _ -> raise Not_found)

let module_NAME base =
  let str_or_safe s = function
    | Tstr _ -> Tstr s
    | Tsafe _ -> Tsafe s
    | _ -> assert false
  in
  let get_particle s =
    Mutil.get_particle (Geneweb_db.Driver.base_particles base) s
  in
  let particle =
    func_arg1_no_kw (function
      | (Tstr s | Tsafe s) as x -> (
          match get_particle s with
          | "" -> Tnull
          | s -> str_or_safe (String.trim s) x)
      | _ -> assert false)
  in
  let without_particle =
    func_arg1_no_kw (function
      | (Tstr s | Tsafe s) as x -> (
          match get_particle s with
          | "" -> Tstr s
          | part ->
              let l = String.length part in
              let s = String.sub s l (String.length s - l) in
              str_or_safe s x)
      | _ -> assert false)
  in
  let lower =
    func_arg1_no_kw (function
      | (Tstr s | Tsafe s) as x -> str_or_safe (Name.lower s) x
      | _ -> assert false)
  in
  Tpat
    (function
    | "lower" -> lower
    | "particle" -> particle
    | "without_particle" -> without_particle
    | _ -> raise Not_found)

let mk_conf conf =
  let lazy_env fn e =
    Tlazy (lazy (Tobj (List.map (fun (k, v) -> (k, fn v)) e)))
  in
  let wizard = Tbool conf.Config.wizard in
  let friend = Tbool conf.friend in
  let command = Tsafe conf.command in
  let env = lazy_env (fun s -> encoded s) conf.env in
  let senv = lazy_env (fun s -> encoded s) conf.senv in
  let henv = lazy_env (fun s -> encoded s) conf.henv in
  let benv = lazy_env (fun s -> Tstr s) conf.base_env in
  let today = mk_dmy conf.today in
  let images_prefix = Tsafe conf.images_prefix in
  let user = Tstr conf.user in
  let lang = Tstr conf.lang in
  Tpat
    (function
    | "benv" -> benv
    | "command" -> command
    | "env" -> env
    | "friend" -> friend
    | "henv" -> henv
    | "images_prefix" -> images_prefix
    | "lang" -> lang
    | "senv" -> senv
    | "today" -> today
    | "user" -> user
    | "wizard" -> wizard
    | _ -> raise Not_found)

let mk_env_no_base conf =
  let prefix = escaped (Util.commd conf) in
  let prefix_base = escaped (Util.prefix_base_password conf) in
  Tpat
    (function
    | "prefix" -> prefix
    | "prefix_base" -> prefix_base
    | x -> Tstr (Mutil.decode @@ List.assoc x conf.env))

let mk_env conf base =
  let prefix = escaped (Util.commd conf) in
  let prefix_base = escaped (Util.prefix_base_password conf) in
  let sosa_ref =
    box_lazy
    @@ lazy
         (match Util.p_getenv conf.Config.env "iz" with
         | Some i -> get_n_mk_person conf base (Driver.Iper.of_string i)
         | None -> (
             match Util.p_getenv conf.env "pz" with
             | None -> (
                 match List.assoc_opt "default_sosa_ref" conf.base_env with
                 | Some n when n <> "" -> (
                     match Gutil.person_ht_find_all base n with
                     | [ ip ] -> get_n_mk_person conf base ip
                     | _ -> Tnull)
                 | _ -> Tnull)
             | Some p -> (
                 match Util.p_getenv conf.env "nz" with
                 | None -> Tnull
                 | Some n -> (
                     let occ =
                       Option.value ~default:0 (Util.p_getint conf.env "ocz")
                     in
                     match Geneweb_db.Driver.person_of_key base p n occ with
                     | Some ip -> get_n_mk_person conf base ip
                     | None -> Tnull))))
  in
  Tpat
    (function
    | "prefix" -> prefix
    | "prefix_base" -> prefix_base
    | "sosa_ref" -> sosa_ref
    | x -> Tstr (Mutil.decode @@ List.assoc x conf.env))

let decode_varenv =
  func_arg1_no_kw @@ function
  | Tstr str | Tsafe str -> Tstr (Mutil.decode (Adef.encoded str))
  | x -> Jg_types.failwith_type_error_1 "decode_varenv" x

let encode_varenv =
  func_arg1_no_kw @@ function
  | Tstr str -> encoded (Mutil.encode str)
  | Tsafe str -> safe (Adef.safe str)
  | x -> Jg_types.failwith_type_error_1 "encode_varenv" x

let mk_base base =
  Tpat
    (function
    | "nb_persons" -> Tint (Driver.nb_of_persons base)
    | "nb_families" -> Tint (Driver.nb_of_families base)
    | "name" -> Tstr (Driver.bname base)
    | _ -> raise Not_found)

let stringify s =
  Printf.sprintf (if String.contains s '\'' then "\"%s\"" else "'%s'") s

let trans ?(autoescape = true) (conf : Config.config) =
  let trad ~kwargs s i =
    let esc = if autoescape then Jg_utils.escape_html else fun s -> s in
    try
      let s = Hashtbl.find conf.lexicon s in
      let t =
        if Lexicon_parser.need_split s then
          Array.of_list @@ String.split_on_char '/' s
        else [| s |]
      in
      let t =
        Array.map
          (fun t ->
            Lexicon_parser.p_trad (Buffer.create 128) [] @@ Lexing.from_string t)
          t
      in
      let i = if i < 0 || i >= Array.length t then 0 else i in
      let arg s = List.assoc s kwargs in
      let t = Array.unsafe_get t i in
      Tsafe
        (let conv acc = function
           | Lexicon_parser.Str s -> s
           | Arg n ->
               if autoescape then
                 match arg n with
                 | Tsafe s -> s
                 | x -> esc (Jg_runtime.string_of_tvalue x)
               else Jg_runtime.string_of_tvalue (arg n)
           | Declension (c, n) ->
               arg n |> Jg_runtime.string_of_tvalue |> Mutil.decline c |> esc
           | Elision (s1, s2) ->
               let x =
                 try unbox_string @@ arg "elision" with Not_found -> acc
               in
               if
                 x <> ""
                 && Unidecode.decode
                      (fun _ _ -> false)
                      (fun _ -> function
                        | 'A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o'
                        | 'u' ->
                            true
                        | _ -> false)
                      (fun _ -> false)
                      x 0 (String.length x)
               then s2
               else s1
         in
         let rec loop s i =
           if i < 0 then s else loop (conv s (Array.unsafe_get t i) ^ s) (i - 1)
         in
         let len = Array.length t in
         loop (conv "" @@ Array.unsafe_get t @@ (len - 1)) (len - 2)
         |> Util.translate_eval)
    with Not_found -> Tstr (Printf.sprintf "{{%s|trans}}" @@ stringify @@ s)
  in
  Tfun
    (fun ?(kwargs = []) -> function
      | Tint i ->
          let kw = kwargs in
          Tfun
            (fun ?(kwargs = []) s ->
              let kwargs = List.rev_append kwargs kw in
              trad ~kwargs (unbox_string s) i)
      | Tsafe s | Tstr s -> trad ~kwargs s 0
      | x -> Jingoo.Jg_types.failwith_type_error_1 "trans" x)

let log =
  func_arg1_no_kw @@ fun x ->
  print_endline @@ Jg_runtime.string_of_tvalue x;
  Tnull

let alphabetic =
  func_arg2_no_kw @@ fun a b ->
  let str = function
    | Tsafe b | Tstr b -> b
    | Tnull -> ""
    | _ -> failwith_type_error_2 "alphabetic" a b
  in
  Tint (Utf8.compare (str a) (str b))

let module_CAST =
  let string =
    func_arg1_no_kw @@ function
    | Tstr _ as s -> s
    | Tsafe s -> Tstr s
    | Tnull -> Tstr ""
    | Tint i -> Tstr (string_of_int i)
    | Tfloat f -> Tstr (string_of_float f)
    | Tbool b -> Tstr (string_of_bool b)
    | x -> failwith_type_error_1 "CAST.string" x
  in
  let int =
    func_arg1_no_kw @@ function
    | Tint _ as i -> i
    | Tnull -> Tint 0
    | Tstr i | Tsafe i -> Tint (int_of_string i)
    | Tfloat f -> Tint (int_of_float f)
    | Tbool true -> Tint 1
    | Tbool false -> Tint 0
    | x -> failwith_type_error_1 "CAST.int" x
  in
  Tpat (function "int" -> int | "string" -> string | _ -> raise Not_found)

let module_GWDB conf base =
  let poi =
    func_arg1_no_kw @@ function
    | Tstr i | Tsafe i -> get_n_mk_person conf base (Driver.Iper.of_string i)
    | x -> failwith_type_error_1 "GWDB.poi" x
  in
  let foi =
    func_arg1_no_kw @@ function
    | Tstr i | Tsafe i ->
        let i = Driver.Ifam.of_string i in
        get_n_mk_family conf base i (Driver.foi base i)
    | x -> failwith_type_error_1 "GWDB.foi" x
  in
  Tpat (function "poi" -> poi | "foi" -> foi | _ -> raise Not_found)

let escape =
  Tfun
    (fun ?kwargs:_ -> function
      | Tsafe s | Tstr s -> Tstr (Jg_utils.escape_html s)
      | x -> Jingoo.Jg_types.failwith_type_error_1 "escape" x)

let default_env_aux conf =
  [
    ("trans", trans conf);
    ("trans_no_escape", trans ~autoescape:false conf);
    ("escape", escape);
    ("DATE", module_DATE conf);
    ("OPT", module_OPT);
    ("decode_varenv", decode_varenv);
    ("encode_varenv", encode_varenv);
    ("alphabetic", alphabetic);
    ("conf", mk_conf conf);
    ("LOG", log);
    ("CAST", module_CAST);
  ]

let default_env_no_base conf =
  ("env", mk_env_no_base conf) :: default_env_aux conf

let default_env conf base =
  ("NAME", module_NAME base)
  :: ("GWDB", module_GWDB conf base)
  :: ("env", mk_env conf base)
  :: ("base", mk_base base)
  :: default_env_aux conf
