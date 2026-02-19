let person_ht = Hashtbl.create 32
let mk_opt fn = function None -> Jingoo.Jg_types.Tnull | Some x -> fn x
let safe (x : Adef.safe_string) = Jingoo.Jg_types.Tsafe (x :> string)
let encoded (x : Adef.encoded_string) = Jingoo.Jg_types.Tsafe (x :> string)
let escaped (x : Adef.escaped_string) = Jingoo.Jg_types.Tsafe (x :> string)

let unbox_string = function
  | Jingoo.Jg_types.Tsafe s | Jingoo.Jg_types.Tstr s -> s
  | x -> Jingoo.Jg_types.failwith_type_error_1 "unbox_string" x

let mk_source_rs conf base str =
  (Jingoo.Jg_types.Tstr str, safe (Geneweb.Notes.source conf base str))

let mk_note_rs conf base env str =
  ( Jingoo.Jg_types.Tstr str,
    safe (Geneweb.Notes.note ~keep_newlines:true conf base env str) )

let mk_person_note_rs conf base p str =
  ( Jingoo.Jg_types.Tstr str,
    safe (Geneweb.Notes.person_note ~keep_newlines:true conf base p str) )

let mk_place str =
  (Jingoo.Jg_types.Tstr str, escaped (Geneweb.Util.string_of_place str))

let rec date_compare_aux date1 date2 =
  let y1 = field date1 "year" in
  let y2 = field date2 "year" in
  match Jingoo.Jg_runtime.jg_compare y1 y2 with
  | Jingoo.Jg_types.Tint 0 -> compare_month date1 date2
  | x -> x

and compare_month date1 date2 =
  match (field date1 "month", field date2 "month") with
  | Jingoo.Jg_types.Tint 0, Jingoo.Jg_types.Tint 0 -> cmp_prec date1 date2
  | Jingoo.Jg_types.Tint 0, Jingoo.Jg_types.Tint _ ->
      if field date1 "prec" = Jingoo.Jg_types.Tsafe "after" then
        Jingoo.Jg_types.Tint 1
      else cmp_prec date1 date2
  | Jingoo.Jg_types.Tint _, Jingoo.Jg_types.Tint 0 ->
      if field date2 "prec" = Jingoo.Jg_types.Tsafe "after" then
        Jingoo.Jg_types.Tint (-1)
      else cmp_prec date1 date2
  | m1, m2 -> (
      match Jingoo.Jg_runtime.jg_compare m1 m2 with
      | Jingoo.Jg_types.Tint 0 -> compare_day date1 date2
      | x -> x)

and compare_day date1 date2 =
  match (field date1 "day", field date2 "day") with
  | Jingoo.Jg_types.Tint 0, Jingoo.Jg_types.Tint 0 -> cmp_prec date1 date2
  | Jingoo.Jg_types.Tint 0, Jingoo.Jg_types.Tint _ ->
      if field date1 "prec" = Jingoo.Jg_types.Tsafe "after" then
        Jingoo.Jg_types.Tint 1
      else cmp_prec date1 date2
  | Jingoo.Jg_types.Tint _, Jingoo.Jg_types.Tint 0 ->
      if field date2 "prec" = Jingoo.Jg_types.Tsafe "after" then
        Jingoo.Jg_types.Tint (-1)
      else cmp_prec date1 date2
  | d1, d2 -> (
      match Jingoo.Jg_runtime.jg_compare d1 d2 with
      | Jingoo.Jg_types.Tint 0 -> cmp_prec date1 date2
      | x -> x)

and cmp_prec d1 d2 =
  match (field d1 "prec", field d2 "prec") with
  | ( Jingoo.Jg_types.Tsafe ("sure" | "about" | "maybe"),
      Jingoo.Jg_types.Tsafe ("sure" | "about" | "maybe") ) ->
      Jingoo.Jg_types.Tint 0
  | Jingoo.Jg_types.Tsafe "after", Jingoo.Jg_types.Tsafe "after"
  | Jingoo.Jg_types.Tsafe "before", Jingoo.Jg_types.Tsafe "before" ->
      Jingoo.Jg_types.Tint 0
  | Jingoo.Jg_types.Tsafe "oryear", Jingoo.Jg_types.Tsafe "oryear"
  | Jingoo.Jg_types.Tsafe "yearint", Jingoo.Jg_types.Tsafe "yearint" ->
      date_compare_aux (field d1 "d2") (field d2 "d2")
  | _, Jingoo.Jg_types.Tsafe "after" | Jingoo.Jg_types.Tsafe "before", _ ->
      Jingoo.Jg_types.Tint (-1)
  | Jingoo.Jg_types.Tsafe "after", _ | _, Jingoo.Jg_types.Tsafe "before" ->
      Jingoo.Jg_types.Tint 1
  | _ -> Jingoo.Jg_types.Tint 0

and field = Jingoo.Jg_runtime.jg_obj_lookup

let rec mk_family (conf : Geneweb.Config.config) base
    ((_, fam, (ifath, imoth, ispouse), _) as fcd) =
  let f = Gwxjg_ezgw.Family.father fcd in
  let m = Gwxjg_ezgw.Family.mother fcd in
  let father = lazy_get_n_mk_person conf base f in
  let mother = lazy_get_n_mk_person conf base m in
  let spouse =
    if ifath = ispouse then father
    else if imoth = ispouse then mother
    else Jingoo.Jg_types.Tnull
  in
  let children =
    Jingoo.Jg_types.Tarray
      (Array.map
         (lazy_get_n_mk_person conf base)
         (Gwxjg_ezgw.Family.children fcd))
  in
  let events' = Gwxjg_ezgw.Family.events fcd in
  let events = lazy_list (mk_event conf base) events' in
  let relation =
    match Gwdb.get_relation fam with
    | Def.Married | NoSexesCheckMarried ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_Marriage) events'
    | NotMarried | NoSexesCheckNotMarried ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_NoMarriage) events'
    | Engaged ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_Engage) events'
    | NoMention ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_NoMention) events'
    | MarriageBann ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_MarriageBann)
          events'
    | MarriageContract ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_MarriageContract)
          events'
    | MarriageLicense ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_MarriageLicense)
          events'
    | Pacs -> find_event conf base (Geneweb.Event.Fevent Def.Efam_PACS) events'
    | Residence ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_Residence) events'
  in
  let separation =
    match Gwdb.get_divorce fam with
    | Def.Divorced _ ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_Divorce) events'
    | Separated ->
        find_event conf base (Geneweb.Event.Fevent Def.Efam_Separated) events'
    | NotDivorced -> Jingoo.Jg_types.Tnull
  in
  let ifam = Jingoo.Jg_types.Tstr (Gwxjg_ezgw.Family.ifam fcd) in
  let origin_file =
    Jingoo.Jg_types.Tstr (Gwxjg_ezgw.Family.origin_file conf base fcd)
  in
  let note_raw, note =
    mk_note_rs conf base [] (Gwxjg_ezgw.Family.note conf base fcd)
  in
  let source_raw, source =
    mk_source_rs conf base (Gwxjg_ezgw.Family.sources base fcd)
  in
  Jingoo.Jg_types.Tpat
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

and get_n_mk_family conf base ?(origin = Gwdb.dummy_iper) ifam cpl =
  let ifath = Gwdb.get_father cpl in
  let imoth = Gwdb.get_mother cpl in
  let cpl =
    ( ifath,
      imoth,
      if ifath = origin then imoth else if imoth = origin then ifath else origin
    )
  in
  let m_auth =
    Geneweb.Person.is_visible conf base (Gwdb.poi base ifath)
    && Geneweb.Person.is_visible conf base (Gwdb.poi base imoth)
  in
  mk_family conf base (ifam, Gwdb.foi base ifam, cpl, m_auth)

and date_compare = Jingoo.Jg_types.func_arg2_no_kw date_compare_aux

and date_eq =
  Jingoo.Jg_types.func_arg2_no_kw (fun d1 d2 ->
      Jingoo.Jg_types.Tbool (date_compare_aux d1 d2 = Jingoo.Jg_types.Tint 0))

and dtext_eq =
  Jingoo.Jg_types.func_arg2_no_kw @@ fun d1 d2 ->
  Jingoo.Jg_types.Tbool
    (Jingoo.Jg_runtime.jg_obj_lookup d1 "__str__"
    = Jingoo.Jg_runtime.jg_obj_lookup d2 "__str__")

and mk_dmy { Date.day; month; year; delta; prec } =
  let day = Jingoo.Jg_types.Tint day in
  let month = Jingoo.Jg_types.Tint month in
  let year = Jingoo.Jg_types.Tint year in
  let delta = Jingoo.Jg_types.Tint delta in
  let prec = to_prec prec in
  Jingoo.Jg_types.Tpat
    (function
    | "day" -> day
    | "month" -> month
    | "year" -> year
    | "delta" -> delta
    | "prec" -> prec
    | _ -> raise Not_found)

and mk_date = function
  | Date.Dtext s ->
      Jingoo.Jg_types.Tpat
        (function
        | "__str__" -> Jingoo.Jg_types.Tstr s
        | "__compare__" ->
            Jingoo.Jg_types.func_arg2_no_kw (fun _ _ -> Jingoo.Jg_types.Tint 0)
        | "__eq__" -> dtext_eq
        | "__Dtext__" -> Jingoo.Jg_types.Tbool true
        | _ -> raise Not_found)
  | Dgreg (d, c) ->
      let year = Jingoo.Jg_types.Tint d.Date.year in
      let month = Jingoo.Jg_types.Tint d.Date.month in
      let day = Jingoo.Jg_types.Tint d.Date.day in
      let prec = to_prec d.Date.prec in
      let d2 =
        match d.Date.prec with
        | OrYear d2 | YearInt d2 ->
            mk_dmy
              {
                Date.day = d2.Date.day2;
                month = d2.Date.month2;
                year = d2.Date.year2;
                prec = Date.Sure;
                delta = 0;
              }
        | _ -> Jingoo.Jg_types.Tnull
      in
      let calendar =
        match c with
        | Dgregorian -> Jingoo.Jg_types.Tsafe "Dgregorian"
        | Djulian -> Jingoo.Jg_types.Tsafe "Djulian"
        | Dfrench -> Jingoo.Jg_types.Tsafe "Dfrench"
        | Dhebrew -> Jingoo.Jg_types.Tsafe "Dhebrew"
        | Dislamic -> Jingoo.Jg_types.Tsafe "Dislamic"
      in
      Jingoo.Jg_types.Tpat
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
  let int s =
    match Jingoo.Jg_runtime.jg_obj_lookup d s with
    | Jingoo.Jg_types.Tint i -> i
    | _ -> 0
  in
  {
    Date.day = int "day";
    month = int "month";
    year = int "year";
    prec = of_prec d;
    delta = 0;
  }

and to_dmy2 d =
  let int s =
    match Jingoo.Jg_runtime.jg_obj_lookup d s with
    | Jingoo.Jg_types.Tint i -> i
    | _ -> 0
  in
  {
    Date.day2 = int "day";
    month2 = int "month";
    year2 = int "year";
    delta2 = 0;
  }

and to_prec = function
  | Date.Sure -> Jingoo.Jg_types.Tsafe "sure"
  | About -> Jingoo.Jg_types.Tsafe "about"
  | Maybe -> Jingoo.Jg_types.Tsafe "maybe"
  | Before -> Jingoo.Jg_types.Tsafe "before"
  | After -> Jingoo.Jg_types.Tsafe "after"
  | OrYear _ -> Jingoo.Jg_types.Tsafe "oryear"
  | YearInt _ -> Jingoo.Jg_types.Tsafe "yearint"

and of_prec d =
  match Jingoo.Jg_runtime.jg_obj_lookup d "prec" with
  | Jingoo.Jg_types.Tsafe "sure" -> Date.Sure
  | Jingoo.Jg_types.Tsafe "about" -> About
  | Jingoo.Jg_types.Tsafe "maybe" -> Maybe
  | Jingoo.Jg_types.Tsafe "before" -> Before
  | Jingoo.Jg_types.Tsafe "after" -> After
  | Jingoo.Jg_types.Tsafe "oryear" ->
      OrYear (to_dmy2 @@ Jingoo.Jg_runtime.jg_obj_lookup d "d2")
  | Jingoo.Jg_types.Tsafe "yearint" ->
      YearInt (to_dmy2 @@ Jingoo.Jg_runtime.jg_obj_lookup d "d2")
  | _ -> assert false

and to_gregorian_aux calendar d =
  let d = to_dmy d in
  match calendar with
  | "Dgregorian" -> d
  | "Djulian" -> Date.convert ~from:Djulian ~to_:Dgregorian d
  | "Dfrench" -> Date.convert ~from:Dfrench ~to_:Dgregorian d
  | "Dhebrew" -> Date.convert ~from:Dhebrew ~to_:Dgregorian d
  | "Dislamic" -> Date.convert ~from:Dislamic ~to_:Dgregorian d
  | _ -> assert false

and of_calendar d =
  match Jingoo.Jg_runtime.jg_obj_lookup d "calendar" with
  | Jingoo.Jg_types.Tsafe "Dgregorian" -> Date.Dgregorian
  | Jingoo.Jg_types.Tsafe "Djulian" -> Date.Djulian
  | Jingoo.Jg_types.Tsafe "Dfrench" -> Date.Dfrench
  | Jingoo.Jg_types.Tsafe "Dhebrew" -> Date.Dhebrew
  | Jingoo.Jg_types.Tsafe "Dislamic" -> Date.Dislamic
  | _ -> assert false

and module_DATE conf =
  let now =
    Jingoo.Jg_types.Tvolatile
      (fun () ->
        let now = Unix.gmtime @@ Unix.time () in
        let day = Jingoo.Jg_types.Tint now.tm_mday in
        let month = Jingoo.Jg_types.Tint (now.tm_mon + 1) in
        let year = Jingoo.Jg_types.Tint (now.tm_year + 1900) in
        Jingoo.Jg_types.Tpat
          (function
          | "day" -> day
          | "month" -> month
          | "year" -> year
          | "prec" -> Jingoo.Jg_types.Tsafe "sure"
          | _ -> raise Not_found))
  in
  let death_symbol =
    Jingoo.Jg_types.Tsafe (Geneweb.DateDisplay.death_symbol conf)
  in
  let string_of_date_aux fn =
    Jingoo.Jg_types.func_arg1_no_kw @@ fun d ->
    try safe (Date.Dgreg (to_dmy d, of_calendar d) |> fn conf)
    with e ->
      if
        Jingoo.Jg_runtime.jg_obj_lookup d "__Dtext__"
        = Jingoo.Jg_types.Tbool true
      then Jingoo.Jg_runtime.jg_obj_lookup d "__str__"
      else raise e
  in
  let string_of_date = string_of_date_aux Geneweb.DateDisplay.string_of_date in
  let string_of_ondate =
    string_of_date_aux Geneweb.DateDisplay.string_of_ondate
  in
  let code_french_year =
    Jingoo.Jg_types.func_arg1_no_kw (fun i ->
        Jingoo.Jg_types.Tstr
          (Geneweb.DateDisplay.code_french_year conf
             (Jingoo.Jg_types.unbox_int i)))
  in
  let string_of_age =
    Jingoo.Jg_types.func_arg1_no_kw (fun d ->
        safe (Geneweb.DateDisplay.string_of_age conf (to_dmy d)))
  in
  let sub =
    Jingoo.Jg_types.func_arg2_no_kw (fun d1 d2 ->
        mk_dmy @@ Date.time_elapsed (to_dmy d2) (to_dmy d1))
  in
  let calendar =
    Jingoo.Jg_types.func_arg2_no_kw (fun dst d ->
        let convert fn = mk_dmy @@ fn @@ to_dmy d in
        match unbox_string @@ dst with
        | "Dgregorian" -> convert Fun.id
        | "Djulian" -> convert (Date.convert ~from:Dgregorian ~to_:Djulian)
        | "Dfrench" -> convert (Date.convert ~from:Dgregorian ~to_:Dfrench)
        | "Dhebrew" -> convert (Date.convert ~from:Dgregorian ~to_:Dhebrew)
        | "Dislamic" -> convert (Date.convert ~from:Dgregorian ~to_:Dislamic)
        | s -> failwith @@ "Unknown calendar: " ^ s)
  in
  Jingoo.Jg_types.Tpat
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

and lazy_list :
      'a. ('a -> Jingoo.Jg_types.tvalue) -> 'a list -> Jingoo.Jg_types.tvalue =
 fun fn -> function
  | [] -> Jingoo.Jg_types.Tlist []
  | l -> Jingoo.Jg_types.Tlazy (lazy (Jingoo.Jg_types.Tlist (List.map fn l)))

and lazy_get_n_mk_person conf base i =
  let lp = lazy (get_n_mk_person conf base i) in
  let iper = Jingoo.Jg_types.Tstr (Gwdb.string_of_iper i) in
  Jingoo.Jg_types.Tpat
    (function
    | "iper" -> iper | s -> Jingoo.Jg_types.unbox_pat (Lazy.force lp) s)

and ppget conf base p =
  if not (Geneweb.Person.is_visible conf base p) then
    if conf.use_restrict then
      Gwdb.get_iper p |> Gwdb.empty_person base |> unsafe_mk_person conf base
    else if
      (* TODO should be is_hidden (?) *)
      (conf.hide_private_names && not (conf.wizard || conf.friend))
      || Gwdb.get_access p = Def.Private
    then
      let lazy_p =
        lazy
          (Jingoo.Jg_types.unbox_pat @@ unsafe_mk_semi_public_person conf base p)
      in
      Jingoo.Jg_types.Tpat
        (function
        | "first_name" ->
            Jingoo.Jg_types.Tsafe
              (Geneweb.NameDisplay.hidden_first_name_txt :> string)
        | "surname" ->
            Jingoo.Jg_types.Tsafe
              (Geneweb.NameDisplay.hidden_surname_txt :> string)
        | "name_is_hidden" -> Jingoo.Jg_types.Tbool true
        | "name_is_restricted" -> Jingoo.Jg_types.Tbool true
        | "first_name_aliases" | "surname_aliases" -> Jingoo.Jg_types.Tlist []
        | x -> (Lazy.force lazy_p) x)
    else unsafe_mk_semi_public_person conf base p
  else unsafe_mk_person conf base p

and pget conf base ip =
  if ip = Gwdb.dummy_iper then
    unsafe_mk_person conf base (Gwdb.empty_person base ip)
  else ppget conf base (Gwdb.poi base ip)

and get_n_mk_person conf base (i : Gwdb.iper) =
  match Hashtbl.find_opt person_ht i with
  | Some p -> p
  | None ->
      let p = pget conf base i in
      Hashtbl.add person_ht i p;
      p

and mk_rparent_aux kind conf base acc =
  let mk_rel i t s =
    let iper = Jingoo.Jg_types.Tstr (Gwdb.string_of_iper i) in
    let kind = kind t in
    let sources = Jingoo.Jg_types.Tstr (Gwdb.sou base s) in
    let lp = lazy (get_n_mk_person conf base i) in
    Jingoo.Jg_types.Tpat
      (function
      | "source" -> sources
      | "kind" -> kind
      | "iper" -> iper
      | s -> Jingoo.Jg_types.unbox_pat (Lazy.force lp) s)
  in
  function
  | { Def.r_fath = None; r_moth = Some i; r_type; r_sources }
  | { r_fath = Some i; r_moth = None; r_type; r_sources } ->
      mk_rel i r_type r_sources :: acc
  | { r_fath = Some i1; r_moth = Some i2; r_type; r_sources } ->
      mk_rel i1 r_type r_sources :: mk_rel i2 r_type r_sources :: acc
  | _ -> Jingoo.Jg_types.Tnull :: acc

and mk_rparent conf base acc =
  mk_rparent_aux
    (function
      | Def.Adoption -> Jingoo.Jg_types.Tsafe "ADOPTIVE_PARENT"
      | Def.CandidateParent -> Jingoo.Jg_types.Tsafe "CANDIDATE_PARENT"
      | Def.FosterParent -> Jingoo.Jg_types.Tsafe "FOSTER_PARENT"
      | Def.GodParent -> Jingoo.Jg_types.Tsafe "GODPARENT"
      | Def.Recognition -> Jingoo.Jg_types.Tsafe "RECOGNIZING_PARENT")
    conf base acc

and mk_rchild conf base acc =
  mk_rparent_aux
    (function
      | Def.Adoption -> Jingoo.Jg_types.Tsafe "ADOPTIVE_CHILD"
      | Def.CandidateParent -> Jingoo.Jg_types.Tsafe "CANDIDATE_CHILD"
      | Def.FosterParent -> Jingoo.Jg_types.Tsafe "FOSTER_CHILD"
      | Def.GodParent -> Jingoo.Jg_types.Tsafe "GODCHILD"
      | Def.Recognition -> Jingoo.Jg_types.Tsafe "RECOGNIZED_CHILD")
    conf base acc

and mk_witness_kind = function
  | Def.Witness -> Jingoo.Jg_types.Tsafe "WITNESS"
  | Def.Witness_GodParent -> Jingoo.Jg_types.Tsafe "WITNESS_GODPARENT"
  | Def.Witness_CivilOfficer -> Jingoo.Jg_types.Tsafe "WITNESS_CIVILOFFICER"
  | Def.Witness_ReligiousOfficer ->
      Jingoo.Jg_types.Tsafe "WITNESS_RELIGIOUSOFFICER"
  | Def.Witness_Informant -> Jingoo.Jg_types.Tsafe "WITNESS_INFORMANT"
  | Def.Witness_Attending -> Jingoo.Jg_types.Tsafe "WITNESS_ATTENDING"
  | Def.Witness_Mentioned -> Jingoo.Jg_types.Tsafe "WITNESS_MENTIONED"
  | Def.Witness_Other -> Jingoo.Jg_types.Tsafe "WITNESS_OTHER"

and mk_pevent conf base e =
  mk_event conf base (Geneweb.Event.event_item_of_pevent e)

and mk_fevent conf base sp e =
  let ei = Geneweb.Event.event_item_of_fevent ~sp e in
  mk_event conf base ei

and mk_fevent' conf base _sp e =
  mk_event conf base (Geneweb.Event.event_item_of_gen_fevent ~sp:None e)

and mk_pevent' conf base e =
  mk_event conf base (Geneweb.Event.event_item_of_gen_pevent e)

and mk_event conf base (evt : 'a Geneweb.Event.event_item) =
  let date =
    match Gwxjg_ezgw.Event.date evt with
    | Some d -> mk_date d
    | None -> Jingoo.Jg_types.Tnull
  in
  let name = safe (Gwxjg_ezgw.Event.name conf base evt) in
  let spouse =
    match Gwxjg_ezgw.Event.spouse_opt evt with
    | None -> Jingoo.Jg_types.Tnull
    | Some i -> lazy_get_n_mk_person conf base i
  in
  let kind = Jingoo.Jg_types.Tsafe (Gwxjg_ezgw.Event.kind evt) in
  let witnesses =
    match Gwxjg_ezgw.Event.witnesses evt with
    | [||] -> Jingoo.Jg_types.Tarray [||]
    | w ->
        let lw =
          lazy (Array.map (fun (i, _, _) -> get_n_mk_person conf base i) w)
        in
        (* We may want to filter on [ip] or [k] before really accessing the person entity *)
        Jingoo.Jg_types.Tarray
          (Array.mapi
             (fun i (ip, k, wnote) ->
               let kind = mk_witness_kind k in
               let iper = Jingoo.Jg_types.Tstr (Gwdb.string_of_iper ip) in
               let note =
                 safe (Geneweb.Util.safe_html @@ Gwdb.sou base wnote)
               in
               Jingoo.Jg_types.Tpat
                 (function
                 | "kind" -> kind
                 | "iper" -> iper
                 | "note" -> note
                 | s -> Jingoo.Jg_types.unbox_pat (Lazy.force lw).(i) @@ s))
             w)
  in
  let place_raw, place =
    mk_place (Gwdb.sou base (Gwxjg_ezgw.Event.place evt))
  in
  let source_raw, source =
    mk_source_rs conf base (Gwxjg_ezgw.Event.src base evt)
  in
  let note_raw, note =
    mk_note_rs conf base [] (Gwxjg_ezgw.Event.note conf base evt)
  in
  Jingoo.Jg_types.Tpat
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

and mk_title base t =
  let ident = Jingoo.Jg_types.Tstr (Gwdb.sou base t.Def.t_ident) in
  let name =
    match t.t_name with
    | Tmain -> Jingoo.Jg_types.Tstr ""
    | Tname s -> Jingoo.Jg_types.Tstr (Gwdb.sou base s)
    | Tnone -> Jingoo.Jg_types.Tnull
  in
  let place_raw, place = mk_place (Gwdb.sou base t.t_place) in
  let date_start = mk_opt mk_date (Date.od_of_cdate t.t_date_start) in
  let date_end = mk_opt mk_date (Date.od_of_cdate t.t_date_end) in
  let nth = Jingoo.Jg_types.Tint t.t_nth in
  Jingoo.Jg_types.Tpat
    (function
    | "ident" -> ident
    | "name" -> name
    | "place" -> place
    | "place_raw" -> place_raw
    | "date_start" -> date_start
    | "date_end" -> date_end
    | "nth" -> nth
    | _ -> raise Not_found)

and mk_ancestors conf base (p : Gwdb.person) =
  let parents =
    match Gwdb.get_parents p with
    | Some ifam -> Some (lazy (Gwdb.foi base ifam))
    | None -> None
  in
  let mk_parent fn =
    match parents with
    | Some f ->
        Jingoo.Jg_types.Tlazy
          (lazy (get_n_mk_person conf base (fn @@ Lazy.force f)))
    | None -> Jingoo.Jg_types.Tnull
  in
  let parents =
    match parents with
    | None -> Jingoo.Jg_types.Tnull
    | Some f ->
        Jingoo.Jg_types.Tlazy
          (lazy
            (let cpl = Lazy.force f in
             let ifam = Gwdb.get_ifam cpl in
             let ifath = Gwdb.get_father cpl in
             let imoth = Gwdb.get_mother cpl in
             let cpl = (ifath, imoth, Gwdb.dummy_iper) in
             let m_auth =
               Geneweb.Person.is_visible conf base (Gwdb.poi base ifath)
               && Geneweb.Person.is_visible conf base (Gwdb.poi base imoth)
             in
             mk_family conf base (ifam, Gwdb.foi base ifam, cpl, m_auth)))
  in
  let father = mk_parent Gwdb.get_father in
  let mother = mk_parent Gwdb.get_mother in
  (parents, father, mother)

and mk_rparents conf base (p : Gwdb.person) =
  let mkr p r =
    if Gwdb.get_sex p = Def.Female then
      { r with Def.r_fath = None; r_moth = Some (Gwdb.get_iper p) }
    else { r with Def.r_fath = Some (Gwdb.get_iper p); r_moth = None }
  in
  match Gwdb.get_rparents p with
  | [] -> Jingoo.Jg_types.Tlist []
  | r ->
      Jingoo.Jg_types.box_list
      @@ List.fold_left (mk_rparent conf base)
           (List.fold_left
              (fun acc i ->
                let p = Geneweb.Util.pget conf base i in
                List.fold_left
                  (fun acc r -> mk_rchild conf base acc @@ mkr p r)
                  acc (Gwdb.get_rparents p))
              []
           @@ Gwdb.get_related p)
           r

and mk_families_spouses iper conf base (p : Gwdb.person) =
  let lazy_families =
    lazy (Array.map (fun i -> (i, Gwdb.foi base i)) @@ Gwdb.get_family p)
  in
  let families =
    Jingoo.Jg_types.Tlazy
      (lazy
        (Jingoo.Jg_types.Tarray
           (Array.map
              (fun (ifam, cpl) ->
                get_n_mk_family conf base ~origin:iper ifam cpl)
              (Lazy.force lazy_families))))
  in
  let spouses =
    Jingoo.Jg_types.Tlazy
      (lazy
        (Jingoo.Jg_types.Tarray
           (Array.map
              (fun (_, c) ->
                let f = Gwdb.get_father c in
                get_n_mk_person conf base
                  (if f = iper then Gwdb.get_mother c else f))
              (Lazy.force lazy_families))))
  in
  (families, spouses)

and mk_str_lst base istrs =
  Jingoo.Jg_types.Tlist
    (List.map (fun i -> Jingoo.Jg_types.Tstr (Gwdb.sou base i)) istrs)

and unsafe_mk_semi_public_person conf base (p : Gwdb.person) =
  let iper' = Gwdb.get_iper p in
  let access_url = escaped (Geneweb.Util.acces conf base p) in
  let access = Jingoo.Jg_types.Tbool (Geneweb.Util.is_public conf base p) in
  let parents, father, mother = mk_ancestors conf base p in
  let families, spouses = mk_families_spouses iper' conf base p in
  let first_name = Jingoo.Jg_types.Tstr (Gwxjg_ezgw.Person.first_name base p) in
  let first_name_aliases = mk_str_lst base (Gwdb.get_first_names_aliases p) in
  let children =
    lazy_list (get_n_mk_person conf base) (Gwxjg_ezgw.Person.children base p)
  in
  let iper = Jingoo.Jg_types.Tstr (Gwdb.string_of_iper iper') in
  let related = mk_rparents conf base p in
  let siblings_aux fn = lazy_list (get_n_mk_person conf base) (fn base p) in
  let siblings = siblings_aux Gwxjg_ezgw.Person.siblings in
  let half_siblings = siblings_aux Gwxjg_ezgw.Person.half_siblings in
  let sex = Jingoo.Jg_types.Tint (Gwxjg_ezgw.Person.sex p) in
  let surname = Jingoo.Jg_types.Tstr (Gwxjg_ezgw.Person.surname base p) in
  let surname_aliases = mk_str_lst base (Gwdb.get_surnames_aliases p) in
  let events = Jingoo.Jg_types.Tlist [] in
  let name_is_hidden =
    Jingoo.Jg_types.Tbool (Geneweb.Person.is_hidden conf base p)
  in
  let name_is_restricted =
    Jingoo.Jg_types.Tbool (Geneweb.Person.has_restricted_name conf base p)
  in
  Jingoo.Jg_types.Tpat
    (function
    | "access_url" -> access_url
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
    | "name_is_hidden" -> name_is_hidden
    | "name_is_restricted" -> name_is_restricted
    | _ -> raise Not_found)

and get_sosa_person conf base person =
  let sosa = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person in
  if sosa = Sosa.zero then Jingoo.Jg_types.Tnull
  else Jingoo.Jg_types.Tstr (Sosa.to_string sosa)

and find_event conf base x (events : 'a Geneweb.Event.event_item list) =
  match List.find_opt (fun evt -> Geneweb.Event.get_name evt = x) events with
  | Some e -> mk_event conf base e
  | None -> Jingoo.Jg_types.Tnull

and find_events conf base x events =
  match
    List.find_opt (fun ei -> List.mem (Geneweb.Event.get_name ei) x) events
  with
  | Some e -> mk_event conf base e
  | None -> Jingoo.Jg_types.Tnull

and unsafe_mk_person conf base (p : Gwdb.person) =
  let iper' = Gwdb.get_iper p in
  let access_url = escaped (Geneweb.Util.acces conf base p) in
  let access = Jingoo.Jg_types.Tbool (Geneweb.Util.is_public conf base p) in
  let parents, father, mother = mk_ancestors conf base p in
  let families, spouses = mk_families_spouses iper' conf base p in
  let aliases = mk_str_lst base (Gwdb.get_aliases p) in
  let children =
    lazy_list (get_n_mk_person conf base) (Gwxjg_ezgw.Person.children base p)
  in
  let consanguinity =
    Jingoo.Jg_types.Tfloat (Gwxjg_ezgw.Person.consanguinity p)
  in
  let events' =
    Gwxjg_ezgw.Person.events conf base
      (Geneweb.Authorized.Person.make ~conf ~base (Gwdb.get_iper p))
  in
  let events = lazy_list (mk_event conf base) events' in
  let birth = find_event conf base (Geneweb.Event.Pevent Epers_Birth) events' in
  let baptism =
    find_event conf base (Geneweb.Event.Pevent Epers_Baptism) events'
  in
  let death =
    let wrap s =
      Jingoo.Jg_types.Tpat
        (function "reason" -> Jingoo.Jg_types.Tsafe s | _ -> raise Not_found)
    in
    match Gwdb.get_death p with
    | Def.NotDead -> Jingoo.Jg_types.Tnull
    | Death (r, _cd) ->
        let reason =
          match r with
          | Def.Killed -> Jingoo.Jg_types.Tsafe "Killed"
          | Murdered -> Jingoo.Jg_types.Tsafe "Murdered"
          | Executed -> Jingoo.Jg_types.Tsafe "Executed"
          | Disappeared -> Jingoo.Jg_types.Tsafe "Disappeared"
          | Unspecified -> Jingoo.Jg_types.Tsafe "Unspecified"
        in
        let e =
          find_event conf base (Geneweb.Event.Pevent Epers_Death) events'
        in
        Jingoo.Jg_types.Tpat
          (function
          | "reason" -> reason | s -> Jingoo.Jg_runtime.jg_obj_lookup e s)
    | DeadYoung -> wrap "DeadYoung"
    | DeadDontKnowWhen -> wrap "DeadDontKnowWhen"
    | DontKnowIfDead -> wrap "DontKnowIfDead"
    | OfCourseDead -> wrap "OfCourseDead"
  in
  let burial =
    find_events conf base
      [
        Geneweb.Event.Pevent Epers_Burial; Geneweb.Event.Pevent Epers_Cremation;
      ]
      events'
  in
  let first_name = Jingoo.Jg_types.Tstr (Gwxjg_ezgw.Person.first_name base p) in
  let first_name_aliases = mk_str_lst base (Gwdb.get_first_names_aliases p) in
  let image =
    Jingoo.Jg_types.Tstr
      (Geneweb.Image.get_portrait conf base p
      |> Option.fold ~none:"" ~some:Geneweb.Image.src_to_string)
  in
  let iper = Jingoo.Jg_types.Tstr (Gwdb.string_of_iper iper') in
  let linked_page =
    Jingoo.Jg_types.Tlazy
      (lazy
        (let db = Gwdb.read_nldb base in
         let db = Geneweb.Notes.merge_possible_aliases conf db in
         let key =
           let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
           let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
           (fn, sn, Gwdb.get_occ p)
         in
         if
           List.exists
             (fun (pg, (_, il)) ->
               (match pg with
               | Def.NLDB.PgInd ip ->
                   Geneweb.Util.pget conf base ip
                   |> Geneweb.Person.is_visible conf base
               | Def.NLDB.PgFam ifam ->
                   Gwdb.foi base ifam |> Gwdb.get_father
                   |> Geneweb.Util.pget conf base
                   |> Geneweb.Person.is_visible conf base
               | Def.NLDB.PgNotes | Def.NLDB.PgMisc _ | Def.NLDB.PgWizard _ ->
                   true)
               && List.exists (fun (k, _) -> k = key) il)
             db
         then
           Jingoo.Jg_types.Tpat
             (fun s ->
               safe
                 (List.fold_left
                    (Geneweb.Perso.linked_page_text conf base p s key)
                    (Adef.safe "") db))
         else Jingoo.Jg_types.Tnull))
  in
  let titles = lazy_list (mk_title base) (Gwdb.get_titles p) in
  let _, note =
    mk_person_note_rs conf base p (Gwxjg_ezgw.Person.note conf base p)
  in
  let occ = Jingoo.Jg_types.Tint (Gwdb.get_occ p) in
  let occupation_raw, occupation =
    mk_source_rs conf base (Gwdb.sou base @@ Gwdb.get_occupation p)
  in
  let public_name =
    Jingoo.Jg_types.Tstr (Gwdb.sou base @@ Gwdb.get_public_name p)
  in
  let qualifiers = mk_str_lst base (Gwdb.get_qualifiers p) in
  let related = mk_rparents conf base p in
  let relations =
    lazy_list (get_n_mk_person conf base) (Gwxjg_ezgw.Person.relations p)
  in
  let sex = Jingoo.Jg_types.Tint (Gwxjg_ezgw.Person.sex p) in
  let siblings_aux fn = lazy_list (get_n_mk_person conf base) (fn base p) in
  let siblings = siblings_aux Gwxjg_ezgw.Person.siblings in
  let half_siblings = siblings_aux Gwxjg_ezgw.Person.half_siblings in
  let source_raw, source =
    mk_source_rs conf base (Gwdb.sou base @@ Gwdb.get_psources p)
  in
  let surname = Jingoo.Jg_types.Tstr (Gwxjg_ezgw.Person.surname base p) in
  let surname_aliases = mk_str_lst base (Gwdb.get_surnames_aliases p) in
  let sosa = Jingoo.Jg_types.box_lazy @@ lazy (get_sosa_person conf base p) in
  let parent_marriage, father_re_marriages, mother_re_marriages =
    match Gwdb.get_parents p with
    | None ->
        ( Jingoo.Jg_types.Tnull,
          Jingoo.Jg_types.Tlist [],
          Jingoo.Jg_types.Tlist [] )
    | Some ifam ->
        let fam = Gwdb.foi base ifam in
        let father = Gwdb.get_father fam in
        let mother = Gwdb.get_mother fam in
        let parent_marriage =
          match
            List.find_opt
              (fun fe -> Gwdb.get_fevent_name fe = Efam_Marriage)
              (Gwdb.get_fevents fam)
          with
          | None -> Jingoo.Jg_types.Tnull
          | Some ev ->
              mk_event conf base
                (Geneweb.Event.event_item_of_fevent
                   ~sp:(Some (Gwdb.get_mother fam))
                   ev)
        in

        (* needed to make event_item with spouse correctly set *)
        let get_spouse ifam ip =
          let family = Gwdb.foi base ifam in
          let father = Gwdb.get_father family in
          if ip <> father then father else Gwdb.get_mother family
        in

        let other_marriage_events ip =
          Array.to_list (Gwdb.get_family (Gwdb.poi base ip))
          |> List.filter (( <> ) ifam)
          |> List.map (fun ifam ->
                 let fevents = Gwdb.get_fevents (Gwdb.foi base ifam) in
                 let spouse = get_spouse ifam ip in
                 List.filter
                   (fun e -> Gwdb.get_fevent_name e = Efam_Marriage)
                   fevents
                 |> List.map
                      (Geneweb.Event.event_item_of_fevent ~sp:(Some spouse)))
          |> List.concat
        in

        ( parent_marriage,
          lazy_list (mk_event conf base) (other_marriage_events father),
          lazy_list (mk_event conf base) (other_marriage_events mother) )
  in
  let name_is_hidden =
    Jingoo.Jg_types.Tbool (Geneweb.Person.is_hidden conf base p)
  in
  let name_is_restricted =
    Jingoo.Jg_types.Tbool (Geneweb.Person.has_restricted_name conf base p)
  in
  Jingoo.Jg_types.Tpat
    (function
    | "access_url" -> access_url
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
    | "parent_marriage" -> parent_marriage
    | "father_re_marriages" -> father_re_marriages
    | "mother_re_marriages" -> mother_re_marriages
    | "name_is_hidden" -> name_is_hidden
    | "name_is_restricted" -> name_is_restricted
    | _ -> raise Not_found)

(* take optionnal p parameter for spouse things? *)
and mk_warning conf base =
  let get_fam ifam =
    let cpl = Gwdb.foi base ifam in
    let ifath = Gwdb.get_father cpl in
    let imoth = Gwdb.get_mother cpl in
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
  fun (warning : Geneweb.Warning.base_warning) ->
    match warning with
    | Geneweb.Warning.BigAgeBetweenSpouses (f, m, a) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "BigAgeBetweenSpouses";
            unsafe_mk_person conf base f;
            unsafe_mk_person conf base m;
            mk_date (Dgreg (a, Dgregorian));
          ]
    | BirthAfterDeath p ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "BirthAfterDeath";
            unsafe_mk_person conf base p;
          ]
    | IncoherentSex (p, i1, i2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "BirthAfterDeath";
            unsafe_mk_person conf base p;
            Jingoo.Jg_types.Tint i1;
            Jingoo.Jg_types.Tint i2;
          ]
    | ChangedOrderOfChildren (ifam, _descend, before, after) ->
        let bef_d, aft_d = Geneweb.Difference.f before after in
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ChangedOrderOfChildren";
            get_fam ifam;
            Jingoo.Jg_types.Tarray
              (Array.map (get_n_mk_person conf base) before);
            Jingoo.Jg_types.Tarray (Array.map (get_n_mk_person conf base) after);
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool bef_d);
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool aft_d);
          ]
    | ChangedOrderOfMarriages (p, before, after) ->
        let bef_d, aft_d = Geneweb.Difference.f before after in
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ChangedOrderOfMarriages";
            unsafe_mk_person conf base p;
            Jingoo.Jg_types.Tarray (Array.map get_fam before);
            Jingoo.Jg_types.Tarray (Array.map get_fam after);
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool bef_d);
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool aft_d);
          ]
    | ChangedOrderOfFamilyEvents (_ifam, before, after) ->
        let before = array_of_list_map (mk_fevent' conf base None) before in
        let after = array_of_list_map (mk_fevent' conf base None) after in
        let bef_d, aft_d = Geneweb.Difference.f before after in
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ChangedOrderOfFamilyEvents";
            Jingoo.Jg_types.Tarray before;
            Jingoo.Jg_types.Tarray after;
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool bef_d);
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool aft_d);
          ]
    | ChangedOrderOfPersonEvents (_p, before, after) ->
        let before = array_of_list_map (mk_pevent' conf base) before in
        let after = array_of_list_map (mk_pevent' conf base) after in
        let bef_d, aft_d = Geneweb.Difference.f before after in
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ChangedOrderOfPersonEvents";
            Jingoo.Jg_types.Tarray before;
            Jingoo.Jg_types.Tarray after;
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool bef_d);
            Jingoo.Jg_types.Tarray (Array.map Jingoo.Jg_types.box_bool aft_d);
          ]
    | ChildrenNotInOrder (ifam, _descend, elder, x) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ChildrenNotInOrder";
            get_fam ifam;
            unsafe_mk_person conf base elder;
            unsafe_mk_person conf base x;
          ]
    | CloseChildren (ifam, c1, c2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "CloseChildren";
            get_fam ifam;
            unsafe_mk_person conf base c1;
            unsafe_mk_person conf base c2;
          ]
    | DistantChildren (ifam, c1, c2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "DistantChildren";
            get_fam ifam;
            unsafe_mk_person conf base c1;
            unsafe_mk_person conf base c2;
          ]
    | DeadOld (p, a) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "DeadOld";
            unsafe_mk_person conf base p;
            mk_date (Dgreg (a, Dgregorian));
          ]
    | DeadTooEarlyToBeFather (father, child) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "DeadTooEarlyToBeFather";
            unsafe_mk_person conf base father;
            unsafe_mk_person conf base child;
          ]
    | FEventOrder (p, e1, e2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "FEventOrder";
            unsafe_mk_person conf base p;
            mk_fevent' conf base None e1;
            mk_fevent' conf base None e2;
          ]
    | FWitnessEventAfterDeath (p, e, ifam) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "FWitnessEventAfterDeath";
            unsafe_mk_person conf base p;
            mk_fevent' conf base None e;
            get_fam ifam;
          ]
    | FWitnessEventBeforeBirth (p, e, ifam) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "FWitnessEventBeforeBirth";
            unsafe_mk_person conf base p;
            mk_fevent' conf base None e;
            get_fam ifam;
          ]
    | IncoherentAncestorDate (p1, p2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "IncoherentAncestorDate";
            unsafe_mk_person conf base p1;
            unsafe_mk_person conf base p2;
          ]
    | MarriageDateAfterDeath p ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "MarriageDateAfterDeath";
            unsafe_mk_person conf base p;
          ]
    | MarriageDateBeforeBirth p ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "MarriageDateBeforeBirth";
            unsafe_mk_person conf base p;
          ]
    | MotherDeadBeforeChildBirth (p1, p2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "MotherDeadBeforeChildBirth";
            unsafe_mk_person conf base p1;
            unsafe_mk_person conf base p2;
          ]
    | OldForMarriage (p, a, i) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "OldForMarriage";
            unsafe_mk_person conf base p;
            mk_date (Dgreg (a, Dgregorian));
            get_n_mk_family conf base i (Gwdb.foi base i);
          ]
    | ParentBornAfterChild (p1, p2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ParentBornAfterChild";
            unsafe_mk_person conf base p1;
            unsafe_mk_person conf base p2;
          ]
    | ParentTooOld (p, a, c) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ParentTooOld";
            unsafe_mk_person conf base p;
            mk_date (Dgreg (a, Dgregorian));
            unsafe_mk_person conf base c;
          ]
    | ParentTooYoung (p, a, c) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "ParentTooYoung";
            unsafe_mk_person conf base p;
            mk_date (Dgreg (a, Dgregorian));
            unsafe_mk_person conf base c;
          ]
    | PEventOrder (p, e1, e2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "PEventOrder";
            unsafe_mk_person conf base p;
            mk_pevent' conf base e1;
            mk_pevent' conf base e2;
          ]
    | PWitnessEventAfterDeath (p, e, origin) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "PWitnessEventAfterDeath";
            unsafe_mk_person conf base p;
            mk_pevent' conf base e;
            unsafe_mk_person conf base origin;
          ]
    | PWitnessEventBeforeBirth (p, e, origin) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "PWitnessEventBeforeBirth";
            unsafe_mk_person conf base p;
            mk_pevent' conf base e;
            unsafe_mk_person conf base origin;
          ]
    | TitleDatesError (p, t) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "TitleDatesError";
            unsafe_mk_person conf base p;
            mk_title base t;
          ]
    | UndefinedSex p ->
        Jingoo.Jg_types.Tset
          [ Jingoo.Jg_types.Tsafe "UndefinedSex"; unsafe_mk_person conf base p ]
    | YoungForMarriage (p, a, i) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "YoungForMarriage";
            unsafe_mk_person conf base p;
            mk_date (Dgreg (a, Dgregorian));
            get_fam i;
          ]
    | PossibleDuplicateFam (ifam1, ifam2) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "PossibleDuplicateFam";
            get_fam ifam1;
            get_fam ifam2;
          ]
    | PossibleDuplicateFamHomonymous (ifam1, ifam2, p) ->
        Jingoo.Jg_types.Tset
          [
            Jingoo.Jg_types.Tsafe "PossibleDuplicateFamHomonymous";
            get_fam ifam1;
            get_fam ifam2;
            unsafe_mk_person conf base p;
          ]

let module_OPT =
  let map =
    Jingoo.Jg_types.func_arg2_no_kw (fun fn -> function
      | Jingoo.Jg_types.Tnull -> Jingoo.Jg_types.Tnull
      | x -> (Jingoo.Jg_types.unbox_fun fn) x)
  in
  Jingoo.Jg_types.Tpat (function "map" -> map | _ -> raise Not_found)

let module_NAME base =
  let str_or_safe s = function
    | Jingoo.Jg_types.Tstr _ -> Jingoo.Jg_types.Tstr s
    | Jingoo.Jg_types.Tsafe _ -> Jingoo.Jg_types.Tsafe s
    | _ -> assert false
  in
  let get_particle s = Mutil.get_particle (Gwdb.base_particles base) s in
  let particle =
    Jingoo.Jg_types.func_arg1_no_kw (function
      | (Jingoo.Jg_types.Tstr s | Jingoo.Jg_types.Tsafe s) as x -> (
          match get_particle s with
          | "" -> Jingoo.Jg_types.Tnull
          | s -> str_or_safe (String.trim s) x)
      | _ -> assert false)
  in
  let without_particle =
    Jingoo.Jg_types.func_arg1_no_kw (function
      | (Jingoo.Jg_types.Tstr s | Jingoo.Jg_types.Tsafe s) as x -> (
          match get_particle s with
          | "" -> Jingoo.Jg_types.Tstr s
          | part ->
              let l = String.length part in
              let s = String.sub s l (String.length s - l) in
              str_or_safe s x)
      | _ -> assert false)
  in
  let lower =
    Jingoo.Jg_types.func_arg1_no_kw (function
      | (Jingoo.Jg_types.Tstr s | Jingoo.Jg_types.Tsafe s) as x ->
          str_or_safe (Name.lower s) x
      | _ -> assert false)
  in
  Jingoo.Jg_types.Tpat
    (function
    | "lower" -> lower
    | "particle" -> particle
    | "without_particle" -> without_particle
    | _ -> raise Not_found)

let mk_conf conf =
  let lazy_env fn e =
    Jingoo.Jg_types.Tlazy
      (lazy (Jingoo.Jg_types.Tobj (List.map (fun (k, v) -> (k, fn v)) e)))
  in
  let wizard = Jingoo.Jg_types.Tbool conf.Geneweb.Config.wizard in
  let friend = Jingoo.Jg_types.Tbool conf.friend in
  let command = Jingoo.Jg_types.Tsafe conf.command in
  let env = lazy_env (fun s -> encoded s) conf.env in
  let senv = lazy_env (fun s -> encoded s) conf.senv in
  let henv = lazy_env (fun s -> encoded s) conf.henv in
  let benv = lazy_env (fun s -> Jingoo.Jg_types.Tstr s) conf.base_env in
  let today = mk_dmy conf.today in
  let image_prefix = Jingoo.Jg_types.Tsafe conf.image_prefix in
  let user = Jingoo.Jg_types.Tstr conf.user in
  let lang = Jingoo.Jg_types.Tstr conf.lang in
  Jingoo.Jg_types.Tpat
    (function
    | "benv" -> benv
    | "command" -> command
    | "env" -> env
    | "friend" -> friend
    | "henv" -> henv
    | "image_prefix" -> image_prefix
    | "lang" -> lang
    | "senv" -> senv
    | "today" -> today
    | "user" -> user
    | "wizard" -> wizard
    | _ -> raise Not_found)

let prefix conf = escaped (Geneweb.Util.commd conf)
let prefix_base conf = escaped (Geneweb.Util.prefix_base_password conf)
let url conf = escaped @@ Geneweb.Util.escape_html conf.Geneweb.Config.command

let mk_env_no_base conf =
  let prefix = prefix conf in
  let prefix_base = prefix_base conf in
  let url = url conf in
  Jingoo.Jg_types.Tpat
    (function
    | "prefix" -> prefix
    | "prefix_base" -> prefix_base
    | "url" -> url
    | x -> Jingoo.Jg_types.Tstr (Mutil.decode @@ List.assoc x conf.env))

let mk_env conf base =
  let prefix = prefix conf in
  let prefix_base = prefix_base conf in
  let url = url conf in
  let sosa_ref =
    Jingoo.Jg_types.box_lazy
    @@ lazy
         (match Geneweb.Util.p_getenv conf.Geneweb.Config.env "iz" with
         | Some i -> get_n_mk_person conf base (Gwdb.iper_of_string i)
         | None -> (
             match Geneweb.Util.p_getenv conf.env "pz" with
             | None -> (
                 match List.assoc_opt "default_sosa_ref" conf.base_env with
                 | Some n when n <> "" -> (
                     match Gutil.person_ht_find_all base n with
                     | [ ip ] -> get_n_mk_person conf base ip
                     | _ -> Jingoo.Jg_types.Tnull)
                 | _ -> Jingoo.Jg_types.Tnull)
             | Some p -> (
                 match Geneweb.Util.p_getenv conf.env "nz" with
                 | None -> Jingoo.Jg_types.Tnull
                 | Some n -> (
                     let occ =
                       Option.value ~default:0
                         (Geneweb.Util.p_getint conf.env "ocz")
                     in
                     match Gwdb.person_of_key base p n occ with
                     | Some ip -> get_n_mk_person conf base ip
                     | None -> Jingoo.Jg_types.Tnull))))
  in
  Jingoo.Jg_types.Tpat
    (function
    | "prefix" -> prefix
    | "prefix_base" -> prefix_base
    | "url" -> url
    | "sosa_ref" -> sosa_ref
    | x -> Jingoo.Jg_types.Tstr (Mutil.decode @@ List.assoc x conf.env))

let decode_varenv =
  Jingoo.Jg_types.func_arg1_no_kw @@ function
  | Jingoo.Jg_types.Tstr str | Jingoo.Jg_types.Tsafe str ->
      Jingoo.Jg_types.Tstr (Mutil.decode (Adef.encoded str))
  | x -> Jingoo.Jg_types.failwith_type_error_1 "decode_varenv" x

let encode_varenv =
  Jingoo.Jg_types.func_arg1_no_kw @@ function
  | Jingoo.Jg_types.Tstr str -> encoded (Mutil.encode str)
  | Jingoo.Jg_types.Tsafe str -> safe (Adef.safe str)
  | x -> Jingoo.Jg_types.failwith_type_error_1 "encode_varenv" x

let mk_base base =
  Jingoo.Jg_types.Tpat
    (function
    | "nb_of_persons" -> Jingoo.Jg_types.Tint (Gwdb.nb_of_persons base)
    | "nb_of_families" -> Jingoo.Jg_types.Tint (Gwdb.nb_of_families base)
    | "name" -> Jingoo.Jg_types.Tstr (Gwdb.bname base)
    | _ -> raise Not_found)

let stringify s =
  Printf.sprintf (if String.contains s '\'' then "\"%s\"" else "'%s'") s

let trans ?(autoescape = true) (conf : Geneweb.Config.config) =
  let trad ~kwargs s i =
    let esc = if autoescape then Jingoo.Jg_utils.escape_html else Fun.id in
    try
      let s = Hashtbl.find conf.lexicon s in
      let t =
        if Gwxjg_lexicon_parser.need_split s then
          Array.of_list @@ String.split_on_char '/' s
        else [| s |]
      in
      let t =
        Array.map
          (fun t ->
            Gwxjg_lexicon_parser.p_trad (Buffer.create 128) []
            @@ Lexing.from_string t)
          t
      in
      let i = if i < 0 || i >= Array.length t then 0 else i in
      let arg s = List.assoc s kwargs in
      let t = Array.unsafe_get t i in
      Jingoo.Jg_types.Tsafe
        (let conv acc = function
           | Gwxjg_lexicon_parser.Str s -> s
           | Arg n ->
               if autoescape then
                 match arg n with
                 | Jingoo.Jg_types.Tsafe s -> s
                 | x -> esc (Jingoo.Jg_runtime.string_of_tvalue x)
               else Jingoo.Jg_runtime.string_of_tvalue (arg n)
           | Declension (c, n) ->
               arg n |> Jingoo.Jg_runtime.string_of_tvalue |> Mutil.decline c
               |> esc
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
         |> Geneweb.Util.translate_eval)
    with Not_found ->
      Jingoo.Jg_types.Tstr (Printf.sprintf "{{%s|trans}}" @@ stringify @@ s)
  in
  Jingoo.Jg_types.Tfun
    (fun ?(kwargs = []) -> function
      | Jingoo.Jg_types.Tint i ->
          let kw = kwargs in
          Jingoo.Jg_types.Tfun
            (fun ?(kwargs = []) s ->
              let kwargs = List.rev_append kwargs kw in
              trad ~kwargs (unbox_string s) i)
      | Jingoo.Jg_types.Tsafe s | Jingoo.Jg_types.Tstr s -> trad ~kwargs s 0
      | x -> Jingoo.Jg_types.failwith_type_error_1 "trans" x)

let log =
  Jingoo.Jg_types.func_arg1_no_kw @@ fun x ->
  print_endline @@ Jingoo.Jg_runtime.string_of_tvalue x;
  Jingoo.Jg_types.Tnull

let alphabetic =
  Jingoo.Jg_types.func_arg2_no_kw @@ fun a b ->
  let str = function
    | Jingoo.Jg_types.Tsafe b | Jingoo.Jg_types.Tstr b -> b
    | Jingoo.Jg_types.Tnull -> ""
    | _ -> Jingoo.Jg_types.failwith_type_error_2 "alphabetic" a b
  in
  Jingoo.Jg_types.Tint (Utf8.compare (str a) (str b))

let module_CAST =
  let string =
    Jingoo.Jg_types.func_arg1_no_kw @@ function
    | Jingoo.Jg_types.Tstr _ as s -> s
    | Jingoo.Jg_types.Tsafe s -> Jingoo.Jg_types.Tstr s
    | Jingoo.Jg_types.Tnull -> Jingoo.Jg_types.Tstr ""
    | Jingoo.Jg_types.Tint i -> Jingoo.Jg_types.Tstr (string_of_int i)
    | Jingoo.Jg_types.Tfloat f -> Jingoo.Jg_types.Tstr (string_of_float f)
    | Jingoo.Jg_types.Tbool b -> Jingoo.Jg_types.Tstr (string_of_bool b)
    | x -> Jingoo.Jg_types.failwith_type_error_1 "CAST.string" x
  in
  let int =
    Jingoo.Jg_types.func_arg1_no_kw @@ function
    | Jingoo.Jg_types.Tint _ as i -> i
    | Jingoo.Jg_types.Tnull -> Jingoo.Jg_types.Tint 0
    | Jingoo.Jg_types.Tstr i | Jingoo.Jg_types.Tsafe i ->
        Jingoo.Jg_types.Tint (int_of_string i)
    | Jingoo.Jg_types.Tfloat f -> Jingoo.Jg_types.Tint (int_of_float f)
    | Jingoo.Jg_types.Tbool true -> Jingoo.Jg_types.Tint 1
    | Jingoo.Jg_types.Tbool false -> Jingoo.Jg_types.Tint 0
    | x -> Jingoo.Jg_types.failwith_type_error_1 "CAST.int" x
  in
  Jingoo.Jg_types.Tpat
    (function "int" -> int | "string" -> string | _ -> raise Not_found)

let module_GWDB conf base =
  let poi =
    Jingoo.Jg_types.func_arg1_no_kw @@ function
    | Jingoo.Jg_types.Tstr i | Jingoo.Jg_types.Tsafe i ->
        get_n_mk_person conf base (Gwdb.iper_of_string i)
    | x -> Jingoo.Jg_types.failwith_type_error_1 "GWDB.poi" x
  in
  let foi =
    Jingoo.Jg_types.func_arg1_no_kw @@ function
    | Jingoo.Jg_types.Tstr i | Jingoo.Jg_types.Tsafe i ->
        let i = Gwdb.ifam_of_string i in
        get_n_mk_family conf base i (Gwdb.foi base i)
    | x -> Jingoo.Jg_types.failwith_type_error_1 "GWDB.foi" x
  in
  Jingoo.Jg_types.Tpat
    (function "poi" -> poi | "foi" -> foi | _ -> raise Not_found)

let escape =
  Jingoo.Jg_types.Tfun
    (fun ?kwargs:_ -> function
      | Jingoo.Jg_types.Tsafe s | Jingoo.Jg_types.Tstr s ->
          Jingoo.Jg_types.Tstr (Jingoo.Jg_utils.escape_html s)
      | x -> Jingoo.Jg_types.failwith_type_error_1 "escape" x)

let comma_separated_of_list =
  Jingoo.Jg_types.func_arg1_no_kw @@ fun l ->
  try
    Jingoo.Jg_types.box_string
      (String.concat ", "
         (List.map Jingoo.Jg_types.unbox_string (Jingoo.Jg_types.unbox_list l)))
  with Invalid_argument _ ->
    Jingoo.Jg_types.failwith_type_error_1 "comma_separated_of_list" l

let assets_version =
  Jingoo.Jg_types.func_arg1_no_kw @@ function
  | Tnull -> (
      match Geneweb.Util.read_assets_version () with
      | Some number -> Jingoo.Jg_types.Tstr (string_of_int number)
      | None -> Jingoo.Jg_types.Tstr "")
  | x -> Jingoo.Jg_types.failwith_type_error_1 "assets_version" x

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
    ("comma_separated_of_list", comma_separated_of_list);
    ("assets_version", assets_version);
  ]

let default_env_no_base conf =
  ("env", mk_env_no_base conf) :: default_env_aux conf

let default_env conf base =
  ("NAME", module_NAME base)
  :: ("GWDB", module_GWDB conf base)
  :: ("env", mk_env conf base)
  :: ("base", mk_base base)
  :: default_env_aux conf
