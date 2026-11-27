open Config
module Driver = Geneweb_db.Driver
module Dict = Geneweb_db.Dict
module Gutil = Geneweb_db.Gutil

let max_scanned = 20_000

let pickable conf base p =
  let fn = Driver.p_first_name base p in
  let sn = Driver.p_surname base p in
  (not (fn = "?" || sn = "?"))
  && ((not (Util.is_hide_names conf p))
     || conf.wizard || conf.friend || Util.is_public conf base p)

let lookup_exact base fn sn oc =
  if fn = "?" || sn = "?" then
    if oc < 0 || oc >= Driver.nb_of_persons base then []
    else
      let p = Driver.poi base (Driver.Iper.of_string (string_of_int oc)) in
      if Driver.p_first_name base p = fn && Driver.p_surname base p = sn then
        [ p ]
      else []
  else
    match Driver.person_of_key base (Name.lower fn) (Name.lower sn) oc with
    | Some ip -> [ Driver.poi base ip ]
    | None -> []

let wiki_key q =
  let len = String.length q in
  if
    len > 4
    && String.starts_with ~prefix:"[[" q
    && String.ends_with ~suffix:"]]" q
  then
    match String.split_on_char '/' (String.sub q 2 (len - 4)) with
    | fn :: sn :: rest ->
        let oc =
          match rest with
          | o :: _ ->
              Option.value ~default:0 (int_of_string_opt (String.trim o))
          | [] -> 0
        in
        Some (String.trim fn, String.trim sn, oc)
    | [ _ ] | [] -> None
  else None

let split_year q =
  match String.rindex_opt q ' ' with
  | Some i ->
      let y = String.sub q (i + 1) (String.length q - i - 1) in
      if y <> "" && String.for_all (fun c -> c >= '0' && c <= '9') y then
        (String.sub q 0 i, y)
      else (q, "")
  | None -> (q, "")

let words s = List.filter (( <> ) "") (String.split_on_char ' ' (Name.lower s))

let first_names_match ws fn =
  let names = words fn in
  List.for_all (fun w -> List.exists (String.starts_with ~prefix:w) names) ws

let year_matches conf base y p =
  y = ""
  || Util.authorized_age conf base p
     &&
     let birth, death, _ = Gutil.get_birth_death_date p in
     List.exists
       (function
         | Some (Adef.Dgreg ({ Adef.year; _ }, _)) ->
             String.starts_with ~prefix:y (string_of_int year)
         | Some (Adef.Dtext _) | None -> false)
       [ birth; death ]

let search conf base ~limit q =
  let q, year = split_year (String.trim q) in
  let sn, fn =
    match String.index_opt q '/' with
    | Some i ->
        ( String.trim (String.sub q 0 i),
          String.sub q (i + 1) (String.length q - i - 1) )
    | None -> (String.trim q, "")
  in
  let ws = words fn in
  let d, prefix =
    if Driver.dict_has base Dict.Surname_search then
      (Dict.Surname_search, Dict.search_key (Driver.base_particles base) sn)
    else (Dict.Surname, sn)
  in
  let pick ip =
    match Util.pget_opt conf base ip with
    | Some p
      when pickable conf base p
           && first_names_match ws (Driver.p_first_name base p)
           && year_matches conf base year p ->
        Some p
    | Some _ | None -> None
  in
  let order p = (Name.lower (Driver.p_first_name base p), Driver.get_occ p) in
  let rec loop scanned n acc = function
    | [] -> (List.rev acc, false)
    | istr :: l ->
        let ipers = fst (Driver.dict_records base d istr) in
        let found =
          List.sort
            (fun p1 p2 -> compare (order p1) (order p2))
            (List.filter_map pick ipers)
        in
        let n = n + List.length found in
        let acc = List.rev_append found acc in
        let scanned = scanned + List.length ipers in
        if n > limit then
          (List.filteri (fun i _ -> i < limit) (List.rev acc), true)
        else if scanned > max_scanned then (List.rev acc, l <> [])
        else loop scanned n acc l
  in
  if String.length prefix < 2 then ([], false)
  else loop 0 0 [] (Driver.dict_entries base d prefix)

let person_json ?(rest = []) conf base p =
  let fn = Driver.p_first_name base p in
  let sn = Driver.p_surname base p in
  let oc = Driver.get_occ p in
  let key = Image.key_dir_basename fn sn oc in
  let has_keydir =
    let dir = Filename.concat (Layout.images conf.layout) key in
    try Sys.is_directory dir with Sys_error _ -> false
  in
  let dates = DateDisplay.short_dates_text_notag conf base p in
  let label =
    let base_label =
      fn ^ " " ^ sn ^ if oc <> 0 then "." ^ string_of_int oc else ""
    in
    if dates = "" then base_label else base_label ^ " " ^ dates
  in
  `Assoc
    (("fn", `String fn)
    :: ("sn", `String sn)
    :: ("oc", `Int oc)
    :: ("key", `String key)
    :: ("label", `String label)
    :: ("has_keydir", `Bool has_keydir)
    :: rest)

let ind_fields ?(rest = []) conf base p =
  ("i", `String (Driver.Iper.to_string (Driver.get_iper p)))
  :: ("acc", `String (Util.acces conf base p :> string))
  :: ( "name",
       `String
         (Util.gen_person_text ~escape:false ~html:false conf base p :> string)
     )
  :: ("dates", `String (DateDisplay.short_dates_text_notag conf base p))
  :: rest

let slash_dates ?(rest = []) conf base p =
  let slash = function
    | Some d -> (DateDisplay.string_slash_of_date conf d :> string)
    | None -> ""
  in
  let auth = Util.authorized_age conf base p in
  let birth = if auth then Date.od_of_cdate (Driver.get_birth p) else None in
  let death =
    match Driver.get_death p with
    | Def.Death (_, d) when auth -> Some (Date.date_of_cdate d)
    | _ -> None
  in
  ("birth", `String (slash birth)) :: ("death", `String (slash death)) :: rest

let pick_json ?(rest = []) conf base p =
  person_json
    ~rest:(ind_fields ~rest:(slash_dates ~rest conf base p) conf base p)
    conf base p

let relatives conf base p =
  let ind ip =
    match Util.pget_opt conf base ip with
    | Some r when not (Util.is_empty_name r) ->
        Some
          (if pickable conf base r then pick_json conf base r
           else `Assoc (ind_fields conf base r))
    | Some _ | None -> None
  in
  let father, mother =
    match Driver.get_parents p with
    | Some ifam ->
        let f = Driver.foi base ifam in
        (ind (Driver.get_father f), ind (Driver.get_mother f))
    | None -> (None, None)
  in
  let ip = Driver.get_iper p in
  let spouses =
    Array.fold_right
      (fun ifam l ->
        match ind (Gutil.spouse ip (Driver.foi base ifam)) with
        | Some j -> j :: l
        | None -> l)
      (Driver.get_family p) []
  in
  let opt = function Some j -> j | None -> `Null in
  [ ("father", opt father); ("mother", opt mother); ("spouses", `List spouses) ]

let search_json conf base =
  let q = Util.p_getenv conf.env "q" |> Option.value ~default:"" in
  let limit =
    match Util.p_getenv conf.env "n" with
    | Some s -> ( try max 1 (min 200 (int_of_string s)) with _ -> 20)
    | None -> 20
  in
  let persons, truncated =
    match wiki_key (String.trim q) with
    | Some (fn, sn, oc) ->
        (List.filter (pickable conf base) (lookup_exact base fn sn oc), false)
    | None -> search conf base ~limit q
  in
  `Assoc
    [
      ( "persons",
        `List
          (List.map
             (fun p -> pick_json ~rest:(relatives conf base p) conf base p)
             persons) );
      ("truncated", `Bool truncated);
    ]

let lookup_print conf base =
  let list l = `List (List.map (person_json conf base) l) in
  let json =
    match Util.p_getenv conf.env "exact" with
    | Some "1" ->
        let g k = Util.p_getenv conf.env k |> Option.value ~default:"" in
        let oc = try int_of_string (g "oc") with _ -> 0 in
        list (lookup_exact base (g "fn") (g "sn") oc)
    | _ -> (
        match Util.p_getenv conf.env "keys" with
        | Some keys when keys <> "" ->
            (* Batch mode: parse "fn1.oc1.sn1;fn2.oc2.sn2;..." like ListImages *)
            list
              (String.split_on_char ';' keys
              |> List.filter_map (fun spec ->
                  match String.split_on_char '.' spec with
                  | [ fn; oc; sn ] -> (
                      let oc_int = try int_of_string oc with _ -> 0 in
                      match lookup_exact base fn sn oc_int with
                      | [ p ] -> Some p
                      | _ -> None)
                  | _ -> None))
        | _ -> search_json conf base)
  in
  Output.status conf Geneweb_http.Code.OK;
  Output.header conf "Content-type: application/json; charset=utf-8";
  Output.header conf "Connection: close";
  Output.print_sstring conf (Yojson.Basic.to_string json)
