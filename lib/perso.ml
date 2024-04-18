(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open TemplAst
open Util

let max_im_wid = 240
let round_2_dec x = floor ((x *. 100.0) +. 0.5) /. 100.0

let string_of_marriage_text conf base fam =
  let marriage = Date.od_of_cdate (get_marriage fam) in
  let marriage_place = sou base (get_marriage_place fam) in
  let s =
    match marriage with
    | Some d -> " " ^<^ DateDisplay.string_of_ondate conf d
    | None -> Adef.safe ""
  in
  match marriage_place with
  | "" -> s
  | _ ->
      s ^^^ ", "
      ^<^ Util.safe_html (string_with_macros conf [] marriage_place)
      ^>^ ","

let string_of_title ?(safe = false) ?(link = true) conf base
    (and_txt : Adef.safe_string) p (nth, name, title, places, dates) =
  let safe_html = if not safe then Util.safe_html else Adef.safe in
  let escape_html = if not safe then Util.escape_html else Adef.escaped in
  let place, places_tl =
    match places with
    | [] -> (Gwdb.empty_string, [])
    | place :: places_tl -> (place, places_tl)
  in
  let acc = safe_html (sou base title ^ " " ^ sou base place) in
  let href place s =
    if link then
      let href =
        "m=TT&sm=S&t="
        ^<^ Mutil.encode (sou base title)
        ^^^ "&p="
        ^<^ Mutil.encode (sou base place)
      in
      geneweb_link conf (href : Adef.encoded_string :> Adef.escaped_string) s
    else s
  in
  let acc = href place acc in
  let rec loop acc places =
    let acc =
      match places with
      | [] -> acc
      | [ _ ] -> acc ^^^ " " ^<^ and_txt ^^^ Adef.safe " "
      | _ -> acc ^>^ ", "
    in
    match places with
    | place :: places ->
        let acc = acc ^^^ href place (safe_html (sou base place)) in
        loop acc places
    | _ -> acc
  in
  let acc = loop acc places_tl in
  let paren =
    match (nth, dates, name) with
    | n, _, _ when n > 0 -> true
    | _, _, Tname _ -> true
    | _, (Some _, _) :: _, _ -> authorized_age conf base p
    | _ -> false
  in
  let acc = if paren then acc ^>^ " (" else acc in
  let first = nth <= 0 in
  let acc =
    if first then acc
    else
      acc
      ^>^ if nth >= 100 then string_of_int nth else transl_nth conf "nth" nth
  in
  let acc, first =
    match name with
    | Tname n ->
        let acc = if not first then acc ^>^ " ," else acc in
        (acc ^^^ (sou base n |> escape_html :> Adef.safe_string), false)
    | _ -> (acc, first)
  in
  let acc =
    if authorized_age conf base p && dates <> [ (None, None) ] then
      fst
      @@ List.fold_left
           (fun (acc, first) (date_start, date_end) ->
             let acc = if not first then acc ^>^ ", " else acc in
             let acc =
               match date_start with
               | Some d -> acc ^^^ DateDisplay.string_of_date conf d
               | None -> acc
             in
             let acc =
               match date_end with
               | Some (Dgreg (d, _)) ->
                   if d.month <> 0 then acc ^>^ " - " else acc ^>^ "-"
               | Some (Dtext _) -> acc ^>^ " - "
               | _ -> acc
             in
             let acc =
               match date_end with
               | Some d -> acc ^^^ DateDisplay.string_of_date conf d
               | None -> acc
             in
             (acc, false))
           (acc, first) dates
    else acc
  in
  if paren then acc ^>^ ")" else acc

let name_equiv n1 n2 =
  Futil.eq_title_names eq_istr n1 n2
  || (n1 = Tmain && n2 = Tnone)
  || (n1 = Tnone && n2 = Tmain)

let nobility_titles_list conf base p =
  let titles =
    List.fold_right
      (fun t l ->
        let t_date_start = Date.od_of_cdate t.t_date_start in
        let t_date_end = Date.od_of_cdate t.t_date_end in
        match l with
        | (nth, name, title, place, dates) :: rl
          when (not conf.is_rtl) && nth = t.t_nth && name_equiv name t.t_name
               && eq_istr title t.t_ident && eq_istr place t.t_place ->
            (nth, name, title, place, (t_date_start, t_date_end) :: dates) :: rl
        | _ ->
            ( t.t_nth,
              t.t_name,
              t.t_ident,
              t.t_place,
              [ (t_date_start, t_date_end) ] )
            :: l)
      (Util.nobtit conf base p) []
  in
  List.fold_right
    (fun (t_nth, t_name, t_ident, t_place, t_dates) l ->
      match l with
      | (nth, name, title, places, dates) :: rl
        when (not conf.is_rtl) && nth = t_nth && name_equiv name t_name
             && eq_istr title t_ident && dates = t_dates ->
          (nth, name, title, t_place :: places, dates) :: rl
      | _ -> (t_nth, t_name, t_ident, [ t_place ], t_dates) :: l)
    titles []

(* ********************************************************************** *)
(*  [Fonc] has_history : config -> string -> bool                         *)

(* ********************************************************************** *)

(** [Description] : Indique si l'individu a été modifiée.
    [Args] :
      - conf   : configuration de la base
      - base   : arbre
      - p      : person
      - p_auth : indique si l'utilisateur est authentifié
    [Retour] : Vrai si la personne a été modifiée, Faux sinon.
    [Rem] : Exporté en clair hors de ce module.                           *)
let has_history conf base p p_auth =
  let fn = sou base (get_first_name p) in
  let sn = sou base (get_surname p) in
  let occ = get_occ p in
  let person_file = HistoryDiff.history_file fn sn occ in
  p_auth && Sys.file_exists (HistoryDiff.history_path conf person_file)

(* ************************************************************************ *)
(*  [Fonc] get_death_text : config -> person -> bool -> string      *)

(* ************************************************************************ *)

(** [Description] : Retourne une description de la mort de la personne
    [Args] :
    - conf : configuration de la base
    - p    : la personne que l'on veut afficher
    - p_auth : authentifié ou non
      [Retour] :
    - string
      [Rem] : Exporté en clair hors de ce module.                             *)
let get_death_text conf p p_auth =
  let died =
    if p_auth then
      let is = index_of_sex (get_sex p) in
      match get_death p with
      | Death (dr, _) -> (
          match dr with
          | Unspecified -> transl_nth conf "died" is |> Adef.safe
          | Murdered -> transl_nth conf "murdered" is |> Adef.safe
          | Killed -> transl_nth conf "killed (in action)" is |> Adef.safe
          | Executed ->
              transl_nth conf "executed (legally killed)" is |> Adef.safe
          | Disappeared -> transl_nth conf "disappeared" is |> Adef.safe)
      | DeadYoung -> transl_nth conf "died young" is |> Adef.safe
      | DeadDontKnowWhen -> transl_nth conf "died" is |> Adef.safe
      | NotDead | DontKnowIfDead | OfCourseDead -> "" |> Adef.safe
    else "" |> Adef.safe
  in
  let on_death_date =
    match (p_auth, get_death p) with
    | true, Death (_, d) -> (
        let d = Date.date_of_cdate d in
        match List.assoc_opt "long_date" conf.base_env with
        | Some "yes" ->
            DateDisplay.string_of_ondate ~link:false conf d
            ^>^ DateDisplay.get_wday conf d
        | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
    | _ -> "" |> Adef.safe
  in
  died ^^^ " " ^<^ on_death_date

let get_baptism_text conf p p_auth =
  let baptized =
    if p_auth then
      get_sex p |> index_of_sex |> transl_nth conf "baptized" |> Adef.safe
    else "" |> Adef.safe
  in
  let on_baptism_date =
    match (p_auth, Date.od_of_cdate (get_baptism p)) with
    | true, Some d -> (
        match List.assoc_opt "long_date" conf.base_env with
        | Some "yes" ->
            DateDisplay.string_of_ondate ~link:false conf d
            ^>^ DateDisplay.get_wday conf d
        | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
    | _ -> "" |> Adef.safe
  in
  baptized ^^^ " " ^<^ on_baptism_date

let get_birth_text conf p p_auth =
  let born =
    if p_auth then
      get_sex p |> index_of_sex |> transl_nth conf "born" |> Adef.safe
    else "" |> Adef.safe
  in
  let on_birth_date =
    match (p_auth, Date.od_of_cdate (get_birth p)) with
    | true, Some d -> (
        match List.assoc_opt "long_date" conf.base_env with
        | Some "yes" ->
            DateDisplay.string_of_ondate ~link:false conf d
            ^>^ DateDisplay.get_wday conf d
        | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
    | _ -> "" |> Adef.safe
  in
  born ^^^ " " ^<^ on_birth_date

let get_marriage_date_text conf fam p_auth =
  match (p_auth, Date.od_of_cdate (get_marriage fam)) with
  | true, Some d -> (
      match List.assoc_opt "long_date" conf.base_env with
      | Some "yes" ->
          DateDisplay.string_of_ondate ~link:false conf d
          ^>^ DateDisplay.get_wday conf d
      | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
  | _ -> "" |> Adef.safe

let get_burial_text conf p p_auth =
  let buried =
    if p_auth then
      get_sex p |> index_of_sex |> transl_nth conf "buried" |> Adef.safe
    else "" |> Adef.safe
  in
  let on_burial_date =
    match get_burial p with
    | Buried cod -> (
        match (p_auth, Date.od_of_cdate cod) with
        | true, Some d -> (
            match List.assoc_opt "long_date" conf.base_env with
            | Some "yes" ->
                DateDisplay.string_of_ondate ~link:false conf d
                ^>^ DateDisplay.get_wday conf d
            | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
        | _ -> "" |> Adef.safe)
    | UnknownBurial | Cremated _ -> "" |> Adef.safe
  in
  buried ^^^ " " ^<^ on_burial_date

let get_cremation_text conf p p_auth =
  let cremated =
    if p_auth then
      get_sex p |> index_of_sex |> transl_nth conf "cremated" |> Adef.safe
    else "" |> Adef.safe
  in
  let on_cremation_date =
    match get_burial p with
    | Cremated cod -> (
        match (p_auth, Date.od_of_cdate cod) with
        | true, Some d -> (
            match List.assoc_opt "long_date" conf.base_env with
            | Some "yes" ->
                DateDisplay.string_of_ondate ~link:false conf d
                ^>^ DateDisplay.get_wday conf d
            | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
        | _ -> "" |> Adef.safe)
    | UnknownBurial | Buried _ -> "" |> Adef.safe
  in
  cremated ^^^ " " ^<^ on_cremation_date

let limit_desc conf =
  match List.assoc_opt "max_desc_level" conf.base_env with
  | Some x when x <> "" -> max 1 (int_of_string x)
  | _ -> 12

let infinite = 10000

let get_descendants_at_level base p lev2 =
  match lev2 with
  | 0 -> []
  | n ->
      (* gather corresponding families in ifam_ht *)
      let ifam_ht = Hashtbl.create 1024 in
      let rec loop lev fam =
        Array.iter
          (fun ifam ->
            if lev < n then
              let children = get_children (foi base ifam) in
              Array.iter
                (fun ch -> loop (lev + 1) (get_family (poi base ch)))
                children
            else Hashtbl.replace ifam_ht ifam ())
          fam
      in
      loop 1 (get_family p);
      (* build the list of descendants from the families *)
      Hashtbl.fold
        (fun ifam () acc ->
          let childrens = get_children (foi base ifam) in
          Array.fold_left (fun acc ch -> ch :: acc) acc childrens)
        ifam_ht []

let make_desc_level_table conf base max_level p =
  let line =
    match p_getenv conf.env "t" with
    | Some "M" -> Male
    | Some "F" -> Female
    | Some _ | None -> Neuter
  in
  (* the table 'levt' may be not necessary, since I added 'flevt'; kept
     because '%max_desc_level;' is still used... *)
  let levt = Gwdb.iper_marker (Gwdb.ipers base) infinite in
  let flevt = Gwdb.ifam_marker (Gwdb.ifams base) infinite in
  let get = pget conf base in
  let ini_ip = get_iper p in
  let rec fill lev = function
    | [] -> ()
    | ipl ->
        let new_ipl =
          List.fold_left
            (fun ipl ip ->
              if Gwdb.Marker.get levt ip <= lev then ipl
              else if lev <= max_level then (
                Gwdb.Marker.set levt ip lev;
                let down =
                  if ip = ini_ip then true
                  else
                    match line with
                    | Male -> get_sex (pget conf base ip) <> Female
                    | Female -> get_sex (pget conf base ip) <> Male
                    | Neuter -> true
                in
                if down then
                  Array.fold_left
                    (fun ipl ifam ->
                      if not (Gwdb.Marker.get flevt ifam <= lev) then
                        Gwdb.Marker.set flevt ifam lev;
                      let ipa = get_children (foi base ifam) in
                      Array.fold_left (fun ipl ip -> ip :: ipl) ipl ipa)
                    ipl
                    (get_family (get ip))
                else ipl)
              else ipl)
            [] ipl
        in
        fill (succ lev) new_ipl
  in
  fill 0 [ ini_ip ];
  (levt, flevt)

let desc_level_max base desc_level_table_l =
  let levt, _ = Lazy.force desc_level_table_l in
  Gwdb.Collection.fold
    (fun acc i ->
      let lev = Gwdb.Marker.get levt i in
      if lev != infinite && acc < lev then lev else acc)
    0 (Gwdb.ipers base)

let max_descendant_level base desc_level_table_l =
  desc_level_max base desc_level_table_l

(* ancestors by list *)

type generation_person =
  | GP_person of Sosa.t * iper * ifam option
  | GP_same of Sosa.t * Sosa.t * iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * iper

let next_generation conf base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
        match gp with
        | GP_person (n, ip, _) -> (
            let n_fath = Sosa.twice n in
            let n_moth = Sosa.inc n_fath 1 in
            let a = pget conf base ip in
            match get_parents a with
            | Some ifam ->
                let cpl = foi base ifam in
                GP_person (n_fath, get_father cpl, Some ifam)
                :: GP_person (n_moth, get_mother cpl, Some ifam)
                :: gpl
            | None -> GP_missing (n, ip) :: gpl)
        | GP_interv None -> gp :: gpl
        | GP_interv (Some (n1, n2, x)) ->
            let x =
              match x with
              | Some (m1, m2) -> Some (Sosa.twice m1, Sosa.twice m2)
              | None -> None
            in
            let gp = GP_interv (Some (Sosa.twice n1, Sosa.twice n2, x)) in
            gp :: gpl
        | GP_same _ | GP_missing _ -> gpl)
      gpl []
  in
  let gpl =
    List.fold_left
      (fun gpl gp ->
        match gp with
        | GP_person (n, ip, _) ->
            let m = Gwdb.Marker.get mark ip in
            if Sosa.eq m Sosa.zero then (
              Gwdb.Marker.set mark ip n;
              gp :: gpl)
            else GP_same (n, m, ip) :: gpl
        | GP_same _ | GP_interv _ | GP_missing _ -> gp :: gpl)
      [] gpl
  in
  List.rev gpl

let next_generation2 conf base mark gpl =
  let gpl =
    List.map
      (fun gp ->
        match gp with
        | GP_same (n, m, _) ->
            GP_interv (Some (n, Sosa.inc n 1, Some (m, Sosa.inc m 1)))
        | GP_person _ | GP_interv _ | GP_missing _ -> gp)
      gpl
  in
  let gpl = next_generation conf base mark gpl in
  List.fold_right
    (fun gp gpl ->
      match (gp, gpl) with
      | GP_interv (Some (n1, n2, x)), GP_interv (Some (n3, n4, y)) :: gpl1 ->
          if Sosa.eq n2 n3 then
            let z =
              match (x, y) with
              | Some (m1, m2), Some (m3, m4) ->
                  if Sosa.eq m2 m3 then Some (m1, m4) else None
              | _ -> None
            in
            GP_interv (Some (n1, n4, z)) :: gpl1
          else GP_interv None :: gpl1
      | GP_interv _, GP_interv _ :: gpl -> GP_interv None :: gpl
      | GP_missing (_, _), gpl -> gpl
      | _ -> gp :: gpl)
    gpl []

let sosa_is_present all_gp n1 =
  let rec loop = function
    | GP_person (n, _, _) :: gpl | GP_same (n, _, _) :: gpl ->
        if Sosa.eq n n1 then true else loop gpl
    | _ :: gpl -> loop gpl
    | [] -> false
  in
  loop all_gp

let get_link all_gp ip =
  let rec loop = function
    | (GP_person (_, ip0, _) as gp) :: gpl ->
        if ip = ip0 then Some gp else loop gpl
    | _ :: gpl -> loop gpl
    | [] -> None
  in
  loop all_gp

let parent_sosa conf base ip all_gp n parent =
  if sosa_is_present all_gp n then Sosa.to_string n
  else
    match get_parents (pget conf base ip) with
    | Some ifam -> (
        match get_link all_gp (parent (foi base ifam)) with
        | Some (GP_person (n, _, _)) -> Sosa.to_string n
        | _ -> "")
    | None -> ""

let will_print = function
  | GP_person (_, _, _) -> true
  | GP_same (_, _, _) -> true
  | GP_interv _ | GP_missing _ -> false

let get_all_generations conf base p =
  let max_level = match p_getint conf.env "v" with Some v -> v | None -> 0 in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
  let rec get_generations level gpll gpl =
    let gpll = gpl :: gpll in
    if level < max_level then
      let next_gpl = next_generation conf base mark gpl in
      if List.exists will_print next_gpl then
        get_generations (level + 1) gpll next_gpl
      else gpll
    else gpll
  in
  let gpll = get_generations 1 [] [ GP_person (Sosa.one, get_iper p, None) ] in
  let gpll = List.rev gpll in
  List.flatten gpll

(* Ancestors by tree:

     8 ? ? ? ? ? ? ?
      4   5   ?   7
        2       3
            1

   1) Build list of levels (t1 = True for parents flag, size 1)
      => [ [8At1 E E] [4Lt1 5Rt1 7At1] [2Lt1 3Rt1] [1Ct1] ]

   2) Enrich list of levels (parents flag, sizing)
      => [ [8At1 E E] [4Lt1 5Rf1 7Af1] [2Lt3 3Rt1] [1Ct5] ]

   3) Display it
       For each cell:
         Top vertical bar if parents flag (not on top line)
         Person
         Person tree link (vertical bar) ) not on bottom line
         Horizontal line                 )
*)

type pos = Left | Right | Center | Alone
type cell = Cell of person * ifam option * pos * bool * int * string | Empty

let rec enrich lst1 lst2 =
  match (lst1, lst2) with
  | _, [] -> []
  | [], lst -> lst
  | Cell (_, _, Right, _, s1, _) :: l1, Cell (p, f, d, u, s2, b) :: l2 ->
      Cell (p, f, d, u, s1 + s2 + 1, b) :: enrich l1 l2
  | Cell (_, _, Left, _, s, _) :: l1, Cell (p, f, d, u, _, b) :: l2 ->
      enrich l1 (Cell (p, f, d, u, s, b) :: l2)
  | Cell (_, _, _, _, s, _) :: l1, Cell (p, f, d, u, _, b) :: l2 ->
      Cell (p, f, d, u, s, b) :: enrich l1 l2
  | Empty :: l1, Cell (p, f, d, _, s, b) :: l2 ->
      Cell (p, f, d, false, s, b) :: enrich l1 l2
  | _ :: l1, Empty :: l2 -> Empty :: enrich l1 l2

let is_empty = List.for_all (( = ) Empty)

let rec enrich_tree lst =
  match lst with
  | [] -> []
  | head :: tail -> (
      if is_empty head then enrich_tree tail
      else
        match tail with
        | [] -> [ head ]
        | thead :: ttail -> head :: enrich_tree (enrich head thead :: ttail))

(* tree_generation_list
    conf: configuration parameters
    base: base name
    gv: number of generations
    p: person *)
let tree_generation_list conf base gv p =
  let mf = match p_getenv conf.env "mf" with Some "1" -> true | _ -> false in
  let next_gen pol =
    List.fold_right
      (fun po l ->
        match po with
        | Empty -> Empty :: l
        | Cell (p, _, _, _, _, base_prefix) -> (
            match get_parents p with
            | Some ifam -> (
                let cpl = foi base ifam in
                let fath =
                  let p = pget conf base (get_father cpl) in
                  if not @@ is_empty_name p then Some p else None
                in
                let moth =
                  let p = pget conf base (get_mother cpl) in
                  if not @@ is_empty_name p then Some p else None
                in
                let fo = Some ifam in
                let base_prefix = conf.bname in
                match (fath, moth) with
                | Some f, Some m ->
                    if mf then
                      Cell (m, fo, Left, true, 1, base_prefix)
                      :: Cell (f, fo, Right, true, 1, base_prefix)
                      :: l
                    else
                      Cell (f, fo, Left, true, 1, base_prefix)
                      :: Cell (m, fo, Right, true, 1, base_prefix)
                      :: l
                | Some f, None -> Cell (f, fo, Alone, true, 1, base_prefix) :: l
                | None, Some m -> Cell (m, fo, Alone, true, 1, base_prefix) :: l
                | None, None -> Empty :: l)
            | None -> (
                match
                  !GWPARAM_ITL.tree_generation_list conf base base_prefix p
                with
                | Some (fath, if1, base_prefix1), Some (moth, if2, base_prefix2)
                  ->
                    if mf then
                      Cell (moth, Some if2, Left, true, 1, base_prefix1)
                      :: Cell (fath, Some if1, Right, true, 1, base_prefix2)
                      :: l
                    else
                      Cell (fath, Some if1, Left, true, 1, base_prefix1)
                      :: Cell (moth, Some if2, Right, true, 1, base_prefix2)
                      :: l
                | Some (fath, ifam, base_prefix), None ->
                    Cell (fath, Some ifam, Alone, true, 1, base_prefix) :: l
                | None, Some (moth, ifam, base_prefix) ->
                    Cell (moth, Some ifam, Alone, true, 1, base_prefix) :: l
                | None, None -> Empty :: l)))
      pol []
  in
  let gen =
    let rec loop i gen l =
      if i = 0 then gen :: l else loop (i - 1) (next_gen gen) (gen :: l)
    in
    loop (gv - 1) [ Cell (p, None, Center, true, 1, conf.bname) ] []
  in
  enrich_tree gen

(* Ancestors surnames list *)

let get_date_place conf base auth_for_all_anc p =
  if auth_for_all_anc || authorized_age conf base p then
    let d1 =
      match Date.od_of_cdate (get_birth p) with
      | None -> Date.od_of_cdate (get_baptism p)
      | x -> x
    in
    let d1 =
      if d1 <> None then d1
      else
        Array.fold_left
          (fun d ifam ->
            if d <> None then d
            else Date.od_of_cdate (get_marriage (foi base ifam)))
          d1 (get_family p)
    in
    let d2 =
      match get_death p with
      | Death (_, cd) -> Some (Date.date_of_cdate cd)
      | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead
        -> (
          match get_burial p with
          | Buried cod | Cremated cod -> Date.od_of_cdate cod
          | UnknownBurial -> None)
    in
    let auth_for_all_anc =
      if auth_for_all_anc then true
      else
        match d2 with
        | Some (Dgreg (d, _)) ->
            let a = Date.time_elapsed d conf.today in
            Util.strictly_after_private_years a conf.private_years_death
        | _ -> false
    in
    let pl =
      let pl = "" in
      let pl = if pl <> "" then pl else sou base (get_birth_place p) in
      let pl = if pl <> "" then pl else sou base (get_baptism_place p) in
      let pl = if pl <> "" then pl else sou base (get_death_place p) in
      let pl = if pl <> "" then pl else sou base (get_burial_place p) in
      if pl <> "" then pl
      else
        Array.fold_left
          (fun pl ifam ->
            if pl <> "" then pl
            else sou base (get_marriage_place (foi base ifam)))
          pl (get_family p)
    in
    ((d1, d2, pl), auth_for_all_anc)
  else ((None, None, ""), false)

(* duplications proposed for merging *)

type dup = DupFam of ifam * ifam | DupInd of iper * iper | NoDup
type excl_dup = (iper * iper) list * (ifam * ifam) list

let gen_excluded_possible_duplications conf s i_of_string =
  match p_getenv conf.env s with
  | Some s ->
      let rec loop ipl i =
        if i >= String.length s then ipl
        else
          let j =
            try String.index_from s i ',' with Not_found -> String.length s
          in
          if j = String.length s then ipl
          else
            let k =
              try String.index_from s (j + 1) ','
              with Not_found -> String.length s
            in
            let s1 = String.sub s i (j - i) in
            let s2 = String.sub s (j + 1) (k - j - 1) in
            let ipl =
              try (i_of_string s1, i_of_string s2) :: ipl with _ -> ipl
            in
            loop ipl (k + 1)
      in
      loop [] 0
  | None -> []

let excluded_possible_duplications conf =
  ( gen_excluded_possible_duplications conf "iexcl" iper_of_string,
    gen_excluded_possible_duplications conf "fexcl" ifam_of_string )

let first_possible_duplication_children iexcl len child eq =
  let rec loop i =
    if i = len then NoDup
    else
      let c1 = child i in
      let rec loop' j =
        if j = len then loop (i + 1)
        else
          let c2 = child j in
          let ic1 = get_iper c1 in
          let ic2 = get_iper c2 in
          if List.mem (ic1, ic2) iexcl then loop' (j + 1)
          else if eq (get_first_name c1) (get_first_name c2) then
            DupInd (ic1, ic2)
          else loop' (j + 1)
      in
      loop' (i + 1)
  in
  loop 0

let first_possible_duplication base ip (iexcl, fexcl) =
  let str =
    let cache = ref [] in
    fun i ->
      match List.assoc_opt i !cache with
      | Some s -> s
      | None ->
          let s = Name.lower @@ sou base i in
          cache := (i, s) :: !cache;
          s
  in
  let eq i1 i2 = str i1 = str i2 in
  let p = poi base ip in
  match get_family p with
  | [||] -> NoDup
  | [| ifam |] ->
      let children = get_children @@ foi base ifam in
      let len = Array.length children in
      if len < 2 then NoDup
      else
        let child i = poi base @@ Array.unsafe_get children i in
        first_possible_duplication_children iexcl len child eq
  | ifams ->
      let len = Array.length ifams in
      let fams = Array.make len None in
      let spouses = Array.make len None in
      let fam i =
        match Array.unsafe_get fams i with
        | Some f -> f
        | None ->
            let f = foi base @@ Array.unsafe_get ifams i in
            Array.unsafe_set fams i (Some f);
            f
      in
      let spouse i =
        match Array.unsafe_get spouses i with
        | Some sp -> sp
        | None ->
            let sp = poi base @@ Gutil.spouse ip @@ fam i in
            Array.unsafe_set spouses i (Some sp);
            sp
      in
      let dup =
        let rec loop i =
          if i = len then NoDup
          else
            let sp1 = spouse i in
            let rec loop' j =
              if j = len then loop (i + 1)
              else
                let sp2 = spouse j in
                if get_iper sp1 = get_iper sp2 then
                  let ifam1 = Array.unsafe_get ifams i in
                  let ifam2 = Array.unsafe_get ifams j in
                  if not (List.mem (ifam2, ifam2) fexcl) then
                    DupFam (ifam1, ifam2)
                  else loop' (j + 1)
                else
                  let isp1 = get_iper sp1 in
                  let isp2 = get_iper sp2 in
                  if List.mem (isp1, isp2) iexcl then loop' (j + 1)
                  else if
                    eq (get_first_name sp1) (get_first_name sp2)
                    && eq (get_surname sp1) (get_surname sp2)
                  then DupInd (isp1, isp2)
                  else loop' (j + 1)
            in
            loop' (i + 1)
        in
        loop 0
      in
      if dup <> NoDup then dup
      else
        let ichildren =
          Array.fold_left Array.append [||]
          @@ Array.init len (fun i -> get_children @@ fam i)
        in
        let len = Array.length ichildren in
        let children = Array.make len None in
        let child i =
          match Array.unsafe_get children i with
          | Some c -> c
          | None ->
              let c = poi base @@ Array.unsafe_get ichildren i in
              Array.unsafe_set children i (Some c);
              c
        in
        first_possible_duplication_children iexcl len child eq

let has_possible_duplications conf base p =
  let ip = get_iper p in
  let excl = excluded_possible_duplications conf in
  first_possible_duplication base ip excl <> NoDup

let merge_date_place conf base surn ((d1, d2, pl), auth) p =
  let (pd1, pd2, ppl), auth = get_date_place conf base auth p in
  let nd1 =
    if pd1 <> None then pd1
    else if eq_istr (get_surname p) surn then if pd2 <> None then pd2 else d1
    else None
  in
  let nd2 =
    if eq_istr (get_surname p) surn then
      if d2 <> None then d2
      else if d1 <> None then d1
      else if pd1 <> None then pd2
      else pd1
    else if pd2 <> None then pd2
    else if pd1 <> None then pd1
    else d1
  in
  let pl =
    if ppl <> "" then ppl else if eq_istr (get_surname p) surn then pl else ""
  in
  ((nd1, nd2, pl), auth)

let build_surnames_list conf base v p =
  let ht = Hashtbl.create 701 in
  let mark =
    let n =
      match List.assoc_opt "max_ancestor_implex" conf.base_env with
      | Some v when v <> "" -> int_of_string v
      | _ -> 5
    in
    Gwdb.iper_marker (Gwdb.ipers base) n
  in
  let auth = conf.wizard || conf.friend in
  let add_surname sosa p surn dp =
    let r =
      try Hashtbl.find ht surn
      with Not_found ->
        let r = ref ((fst dp, p), []) in
        Hashtbl.add ht surn r;
        r
    in
    r := (fst !r, sosa :: snd !r)
  in
  let rec loop lev sosa p surn dp =
    if Gwdb.Marker.get mark (get_iper p) = 0 then ()
    else if lev > v then
      if is_hide_names conf p && not (authorized_age conf base p) then ()
      else add_surname sosa p surn dp
    else (
      Gwdb.Marker.set mark (get_iper p) (Gwdb.Marker.get mark (get_iper p) - 1);
      match get_parents p with
      | Some ifam ->
          let cpl = foi base ifam in
          let fath = pget conf base (get_father cpl) in
          let moth = pget conf base (get_mother cpl) in
          if
            (not (eq_istr surn (get_surname fath)))
            && not (eq_istr surn (get_surname moth))
          then add_surname sosa p surn dp;
          let sosa = Sosa.twice sosa in
          (if not (is_hidden fath) then
           let dp1 = merge_date_place conf base surn dp fath in
           loop (lev + 1) sosa fath (get_surname fath) dp1);
          let sosa = Sosa.inc sosa 1 in
          if not (is_hidden moth) then
            let dp2 = merge_date_place conf base surn dp moth in
            loop (lev + 1) sosa moth (get_surname moth) dp2
      | None -> add_surname sosa p surn dp)
  in
  loop 1 Sosa.one p (get_surname p) (get_date_place conf base auth p);
  let list = ref [] in
  Hashtbl.iter
    (fun i dp ->
      let surn = sou base i in
      if surn <> "?" then list := (surn, !dp) :: !list)
    ht;
  (* TODO don't query db in sort *)
  List.sort
    (fun (s1, _) (s2, _) ->
      match
        Gutil.alphabetic_order
          (surname_without_particle base s1)
          (surname_without_particle base s2)
      with
      | 0 ->
          Gutil.alphabetic_order (surname_particle base s1)
            (surname_particle base s2)
      | x -> x)
    !list

(* ************************************************************************* *)
(* [Fonc] build_list_eclair :
     config -> base -> int -> person ->
       list
         (string * string * option date * option date * person * list iper) *)

(* ************************************************************************* *)

(** [Description] : Construit la liste éclair des ascendants de p jusqu'à la
                    génération v.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - v    : le nombre de génération
      - p    : person
    [Retour] : (surname * place * date begin * date end * person * list iper)
    [Rem] : Exporté en clair hors de ce module.                              *)
let build_list_eclair conf base v p =
  let ht = Hashtbl.create 701 in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
  (* Fonction d'ajout dans la Hashtbl. A la clé (surname, place) on associe *)
  (* la personne (pour l'interprétation dans le template), la possible date *)
  (* de début, la possible date de fin, la liste des personnes/évènements.  *)
  (* Astuce: le nombre d'élément de la liste correspond au nombre             *)
  (* d'évènements et le nombre d'iper unique correspond au nombre d'individu. *)
  let add_surname p surn pl d =
    if not (is_empty_string pl) then
      let pl = Util.string_of_place conf (sou base pl) in
      let r =
        try Hashtbl.find ht (surn, pl)
        with Not_found ->
          let r = ref (p, None, None, []) in
          Hashtbl.add ht (surn, pl) r;
          r
      in
      (* Met la jour le binding : dates et liste des iper. *)
      r :=
        (fun p (pp, db, de, l) ->
          let db =
            match db with
            | Some dd -> (
                match d with
                | Some d -> if Date.compare_date d dd < 0 then Some d else db
                | None -> db)
            | None -> d
          in
          let de =
            match de with
            | Some dd -> (
                match d with
                | Some d -> if Date.compare_date d dd > 0 then Some d else de
                | None -> de)
            | None -> d
          in
          (pp, db, de, get_iper p :: l))
          p !r
  in
  (* Fonction d'ajout de tous les évènements d'une personne (birth, bapt...). *)
  let add_person p surn =
    if Gwdb.Marker.get mark (get_iper p) then ()
    else (
      Gwdb.Marker.set mark (get_iper p) true;
      add_surname p surn (get_birth_place p) (Date.od_of_cdate (get_birth p));
      add_surname p surn (get_baptism_place p)
        (Date.od_of_cdate (get_baptism p));
      let death =
        match get_death p with
        | Death (_, cd) -> Some (Date.date_of_cdate cd)
        | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead
          ->
            None
      in
      add_surname p surn (get_death_place p) death;
      let burial =
        match get_burial p with
        | Buried cod | Cremated cod -> Date.od_of_cdate cod
        | UnknownBurial -> None
      in
      add_surname p surn (get_burial_place p) burial;
      Array.iter
        (fun ifam ->
          let fam = foi base ifam in
          add_surname p surn (get_marriage_place fam)
            (Date.od_of_cdate (get_marriage fam)))
        (get_family p))
  in

  (* TODO do we have a get_ascendants function? *)
  (* Parcours les ascendants de p et les ajoute dans la Hashtbl. *)
  let rec loop lev p =
    let surn = get_surname p in
    if lev > v then
      if is_hide_names conf p && not (authorized_age conf base p) then ()
      else add_person p surn
    else add_person p surn;
    match get_parents p with
    | None -> ()
    | Some ifam ->
        let cpl = foi base ifam in
        let fath = pget conf base (get_father cpl) in
        let moth = pget conf base (get_mother cpl) in
        if not (is_hidden fath) then loop (lev + 1) fath;
        if not (is_hidden moth) then loop (lev + 1) moth
  in
  (* Construction de la Hashtbl. *)
  loop 1 p;

  (* TODO do it at insertion time *)
  (* On parcours la Hashtbl, et on élimine les noms vide (=?) *)
  let l = ref [] in
  Hashtbl.iter
    (fun (istr, place) ht_val ->
      let surn = sou base istr in
      if surn <> "?" then
        let p, db, de, pl = (fun x -> x) !ht_val in
        l := (surn, place, db, de, p, pl) :: !l)
    ht;
  (* On trie la liste par nom, puis lieu. *)
  (* TODO don't query db in sort *)
  List.sort
    (fun (s1, pl1, _, _, _, _) (s2, pl2, _, _, _, _) ->
      match
        Gutil.alphabetic_order
          (surname_without_particle base s1)
          (surname_without_particle base s2)
      with
      | 0 -> (
          match
            Gutil.alphabetic_order (surname_particle base s1)
              (surname_particle base s2)
          with
          | 0 ->
              Gutil.alphabetic_order
                (pl1 : Adef.escaped_string :> string)
                (pl2 : Adef.escaped_string :> string)
          | x -> x)
      | x -> x)
    !l

let linked_page_text conf base p s key (str : Adef.safe_string) (pg, (_, il)) :
    Adef.safe_string =
  match pg with
  | Def.NLDB.PgMisc pg ->
      let l = List.map snd (List.filter (fun (k, _) -> k = key) il) in
      List.fold_right
        (fun text (str : Adef.safe_string) ->
          try
            let nenv, _ = Notes.read_notes base pg in
            let v =
              let v = List.assoc s nenv in
              if v = "" then raise Not_found
              else Util.nth_field v (Util.index_of_sex (get_sex p))
            in
            match text.Def.NLDB.lnTxt with
            | Some "" -> str
            | Some _ | None ->
                let str1 =
                  let v =
                    let text = text.Def.NLDB.lnTxt in
                    match text with
                    | Some text ->
                        let rec loop i len =
                          if i = String.length text then Buff.get len
                          else if text.[i] = '*' then
                            loop (i + 1) (Buff.mstore len v)
                          else loop (i + 1) (Buff.store len text.[i])
                        in
                        loop 0 0
                    | None -> v
                  in
                  let a, b, c =
                    try
                      let i = String.index v '{' in
                      let j = String.index v '}' in
                      let a = String.sub v 0 i in
                      let b = String.sub v (i + 1) (j - i - 1) in
                      let c = String.sub v (j + 1) (String.length v - j - 1) in
                      ( a |> Util.safe_html,
                        b |> Util.safe_html,
                        c |> Util.safe_html )
                    with Not_found ->
                      (Adef.safe "", Util.safe_html v, Adef.safe "")
                  in
                  (a : Adef.safe_string)
                  ^^^ {|<a href="|}
                  ^<^ (commd conf ^^^ {|m=NOTES&f=|}
                       ^<^ (Mutil.encode pg :> Adef.escaped_string)
                       ^>^ {|#p_|}
                       ^ string_of_int text.Def.NLDB.lnPos
                        : Adef.escaped_string
                        :> Adef.safe_string)
                  ^^^ {|">|} ^<^ b ^^^ {|</a>|} ^<^ c
                in
                if (str :> string) = "" then str1 else str ^^^ ", " ^<^ str1
          with Not_found -> str)
        l str
  | Def.NLDB.PgInd _ | Def.NLDB.PgFam _ | Def.NLDB.PgNotes | Def.NLDB.PgWizard _
    ->
      str

let links_to_ind conf base db key =
  let l =
    List.fold_left
      (fun pgl (pg, (_, il)) ->
        let record_it =
          match pg with
          | Def.NLDB.PgInd ip -> authorized_age conf base (pget conf base ip)
          | Def.NLDB.PgFam ifam ->
              authorized_age conf base
                (pget conf base (get_father @@ foi base ifam))
          | Def.NLDB.PgNotes | Def.NLDB.PgMisc _ | Def.NLDB.PgWizard _ -> true
        in
        if record_it then
          List.fold_left
            (fun pgl (k, _) -> if k = key then pg :: pgl else pgl)
            pgl il
        else pgl)
      [] db
  in
  List.sort_uniq compare l

(* Interpretation of template file *)

let rec compare_ls sl1 sl2 =
  match (sl1, sl2) with
  | s1 :: sl1, s2 :: sl2 ->
      (* Je ne sais pas s'il y a des effets de bords, mais on  *)
      (* essaie de convertir s1 s2 en int pour éviter que "10" *)
      (* soit plus petit que "2". J'espère qu'on ne casse pas  *)
      (* les performances à cause du try..with.                *)
      let c =
        try Stdlib.compare (int_of_string s1) (int_of_string s2)
        with Failure _ -> Gutil.alphabetic_order s1 s2
      in
      if c = 0 then compare_ls sl1 sl2 else c
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | [], [] -> 0

module SortedList = Set.Make (struct
  type t = string list

  let compare = compare_ls
end)

(*
   Type pour représenté soit :
     - la liste des branches patronymique
       (surname * date begin * date end * place * person * list sosa * loc)
     - la liste éclair
       (surname * place * date begin * date end * person * list iper * loc)
*)
type ancestor_surname_info =
  | Branch of
      (string * date option * date option * string * person * Sosa.t list * loc)
  | Eclair of
      (string
      * Adef.safe_string
      * date option
      * date option
      * person
      * iper list
      * loc)

type title_item =
  int
  * istr gen_title_name
  * istr
  * istr list
  * (date option * date option) list

type path_mode = Paths_cnt_raw | Paths_cnt | Paths

type 'a env =
  | Vallgp of generation_person list
  | Vanc of generation_person
  | Vanc_surn of ancestor_surname_info
  | Vcell of cell
  | Vcelll of cell list
  | Vcnt of int ref
  | Vcousl of (iper * (ifam list list * iper list * int) * int list) list ref
  | Vcous_level of int ref * int ref
  | Vdesclevtab of ((iper, int) Marker.t * (ifam, int) Marker.t) lazy_t
  | Vdmark of (iper, bool) Marker.t ref
  | Vslist of SortedList.t ref
  | Vslistlm of string list list
  | Vind of person
  | Vfam of ifam * family * (iper * iper * iper) * bool
  | Vrel of relation * person option
  | Vbool of bool
  | Vint of int
  | Vgpl of generation_person list
  | Vnldb of (Gwdb.iper, Gwdb.ifam) Def.NLDB.t
  | Vstring of string
  | Vsosa_ref of person option
  | Vsosa of (iper * (Sosa.t * person) option) list ref
  | Vt_sosa of SosaCache.sosa_t option
  | Vtitle of person * title_item
  | Vvars of (string * string) list ref
  | Vevent of person * istr Event.event_item
  | Vlazyp of string option ref
  | Vlazy of 'a env Lazy.t
  | Vother of 'a
  | Vnone

(** [has_witness_for_event event_name events] is [true] iff there is an event with name [event_name] in [events] and this event had witnesses. It do not check for permissions *)
let has_witness_for_event conf base p event_name =
  List.exists
    (fun ((name, _, _, _, _, wl, _) : istr Event.event_item) ->
      name = event_name && Array.length wl > 0)
    (Event.events conf base p)

let get_env v env =
  try match List.assoc v env with Vlazy l -> Lazy.force l | x -> x
  with Not_found -> Vnone

let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

let extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""

let template_file = ref "perso.txt"

let warning_use_has_parents_before_parent (fname, bp, ep) var r =
  Printf.sprintf
    "%s %d-%d: since v5.00, must test \"has_parents\" before using \"%s\"\n"
    fname bp ep var
  |> !GWPARAM.syslog `LOG_WARNING;
  r

let bool_val x = VVbool x
let str_val x = VVstring x
let null_val = VVstring ""

let safe_val (x : [< `encoded | `escaped | `safe ] Adef.astring) =
  VVstring ((x :> Adef.safe_string) :> string)

let gen_string_of_img_sz max_w max_h conf base (p, p_auth) =
  if p_auth then
    match Image.get_portrait_with_size conf base p with
    | Some (_, Some (w, h)) ->
        let w, h = Image.scale_to_fit ~max_w ~max_h ~w ~h in
        Format.sprintf " width=\"%d\" height=\"%d\"" w h
    | Some (_, None) -> Format.sprintf " height=\"%d\"" max_h
    | None -> ""
  else ""

let string_of_image_size = gen_string_of_img_sz max_im_wid max_im_wid
let string_of_image_medium_size = gen_string_of_img_sz 160 120
let string_of_image_small_size = gen_string_of_img_sz 100 75

let get_sosa conf base env r p =
  try List.assoc (get_iper p) !r
  with Not_found ->
    let s =
      match get_env "sosa_ref" env with
      | Vsosa_ref v -> (
          match get_env "t_sosa" env with
          | Vt_sosa (Some t_sosa) -> SosaCache.find_sosa conf base p v t_sosa
          | _ -> None)
      | _ -> None
    in
    r := (get_iper p, s) :: !r;
    s

(* ************************************************************************** *)
(*  [Fonc] get_linked_page : config -> base -> person -> string -> string     *)

(* ************************************************************************** *)

(** [Description] : Permet de récupérer un lien de la chronique familiale.
    [Args] :
      - conf : configuration
      - base : base de donnée
      - p    : person
      - s    : nom du lien (eg. "HEAD", "OCCU", "BIBLIO", "BNOTE", "DEATH")
    [Retour] : string : "<a href="xxx">description du lien</a>"
    [Rem] : Exporté en clair hors de ce module.                               *)
let get_linked_page conf base p s =
  let db = Gwdb.read_nldb base in
  let db = Notes.merge_possible_aliases conf db in
  let key =
    let fn = Name.lower (sou base (get_first_name p)) in
    let sn = Name.lower (sou base (get_surname p)) in
    (fn, sn, get_occ p)
  in
  List.fold_left (linked_page_text conf base p s key) (Adef.safe "") db

let make_ep conf base ip =
  let p = pget conf base ip in
  let p_auth = authorized_age conf base p in
  (p, p_auth)

let make_efam conf base ip ifam =
  let fam = foi base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let ispouse = if ip = ifath then imoth else ifath in
  let cpl = (ifath, imoth, ispouse) in
  let m_auth =
    authorized_age conf base (pget conf base ifath)
    && authorized_age conf base (pget conf base imoth)
  in
  (fam, cpl, m_auth)

let mode_local env =
  match get_env "fam_link" env with Vfam _ -> false | _ -> true

let get_note_source conf base ?p auth no_note note_source =
  safe_val
  @@
  if auth && not no_note then
    let env =
      match p with
      | None -> []
      | Some p -> [ ('i', fun () -> Image.default_portrait_filename base p) ]
    in
    Notes.source_note_with_env conf base env (sou base note_source)
  else Adef.safe ""

let date_aux conf p_auth date =
  match (p_auth, Date.od_of_cdate date) with
  | true, Some d ->
      if List.assoc_opt "long_date" conf.base_env = Some "yes" then
        DateDisplay.string_of_ondate conf d ^>^ DateDisplay.get_wday conf d
        |> safe_val
      else DateDisplay.string_of_ondate conf d |> safe_val
  | _ -> null_val

let get_marriage_witnesses fam =
  let fevents = Gwdb.get_fevents fam in
  let witnesses = List.map (fun marriage -> marriage.efam_witnesses) fevents in
  witnesses |> Array.concat

let get_nb_marriage_witnesses_of_kind fam wk =
  let witnesses = get_marriage_witnesses fam in
  Array.fold_left
    (fun acc (_, w) -> if wk = w then acc + 1 else acc)
    0 witnesses

let number_of_descendants_aux conf base env all_levels sl eval_int =
  match get_env "level" env with
  | Vint i -> (
      match get_env "desc_level_table" env with
      | Vdesclevtab t ->
          let m = fst (Lazy.force t) in
          let cnt =
            Gwdb.Collection.fold
              (fun cnt ip ->
                if all_levels then
                  if Gwdb.Marker.get m ip <= i then cnt + 1 else cnt
                else if Gwdb.Marker.get m ip = i then cnt + 1
                else cnt)
              0 (Gwdb.ipers base)
          in
          VVstring (eval_int conf (if all_levels then cnt - 1 else cnt) sl)
      | _ -> raise Not_found)
  | _ -> raise Not_found

let rec eval_var conf base env ep loc sl =
  try eval_simple_var conf base env ep sl
  with Not_found -> eval_compound_var conf base env ep loc sl

and eval_simple_var conf base env ep = function
  | [ s ] -> (
      try bool_val (eval_simple_bool_var conf base env s)
      with Not_found -> eval_simple_str_var conf base env ep s)
  | _ -> raise Not_found

and eval_simple_bool_var conf base env =
  let fam_check_aux fn =
    match get_env "fam" env with
    | Vfam (_, fam, _, _) when mode_local env -> fn fam
    | _ -> (
        match get_env "fam_link" env with
        | Vfam (_, fam, _, _) -> fn fam
        | _ -> raise Not_found)
  in
  let check_relation test =
    fam_check_aux (fun fam -> test @@ get_relation fam)
  in
  function
  | "are_divorced" ->
      fam_check_aux (fun fam ->
          match get_divorce fam with
          | Divorced _ -> true
          | NotDivorced | Separated -> false)
  | "are_engaged" -> check_relation (( = ) Engaged)
  | "are_married" ->
      check_relation (function
        | Married | NoSexesCheckMarried -> true
        | _ -> false)
  | "are_not_married" ->
      check_relation (function
        | NotMarried | NoSexesCheckNotMarried -> true
        | _ -> false)
  | "are_pacs" -> check_relation (( = ) Pacs)
  | "are_marriage_banns" -> check_relation (( = ) MarriageBann)
  | "are_marriage_contract" -> check_relation (( = ) MarriageContract)
  | "are_marriage_license" -> check_relation (( = ) MarriageLicense)
  | "are_residence" -> check_relation (( = ) Residence)
  | "are_separated" -> fam_check_aux (fun fam -> get_divorce fam = Separated)
  | "browsing_with_sosa_ref" -> (
      match get_env "sosa_ref" env with
      | Vsosa_ref v -> v <> None
      | _ -> raise Not_found)
  | "has_comment" | "has_fnotes" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          m_auth && (not conf.no_note) && sou base (get_comment fam) <> ""
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, _, _, _) -> false
          | _ -> raise Not_found))
  | "has_fsources" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> m_auth && sou base (get_fsources fam) <> ""
      | _ -> false)
  | "has_marriage_note" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          m_auth && (not conf.no_note) && sou base (get_marriage_note fam) <> ""
      | _ -> raise Not_found)
  | "has_marriage_source" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          m_auth && sou base (get_marriage_src fam) <> ""
      | _ -> raise Not_found)
  | "has_relation_her" -> (
      match get_env "rel" env with
      | Vrel ({ r_moth = Some _ }, None) -> true
      | _ -> false)
  | "has_relation_him" -> (
      match get_env "rel" env with
      | Vrel ({ r_fath = Some _ }, None) -> true
      | _ -> false)
  | "has_witnesses" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          m_auth && Array.length (get_witnesses fam) > 0
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, _, _, _) -> false
          | _ -> raise Not_found))
  | "is_first" -> (
      match get_env "first" env with Vbool x -> x | _ -> raise Not_found)
  | "is_last" -> (
      match get_env "last" env with Vbool x -> x | _ -> raise Not_found)
  | "is_no_mention" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) when mode_local env -> get_relation fam = NoMention
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, _) -> get_relation fam = NoMention
          | _ -> raise Not_found))
  | "is_no_sexes_check" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) when mode_local env ->
          get_relation fam = NoSexesCheckNotMarried
          || get_relation fam = NoSexesCheckMarried
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, _) ->
              get_relation fam = NoSexesCheckNotMarried
              || get_relation fam = NoSexesCheckMarried
          | _ -> raise Not_found))
  | "is_self" -> get_env "pos" env = Vstring "self"
  | "is_sibling_after" -> get_env "pos" env = Vstring "next"
  | "is_sibling_before" -> get_env "pos" env = Vstring "prev"
  | "lazy_printed" -> (
      match get_env "lazy_print" env with
      | Vlazyp r -> !r = None
      | _ -> raise Not_found)
  | s ->
      let v = extract_var "file_exists_" s in
      if v <> "" then SrcfileDisplay.source_file_name conf v |> Sys.file_exists
      else raise Not_found

and eval_simple_str_var conf base env (p, p_auth) = function
  | "alias" -> (
      match get_env "alias" env with
      | Vstring s -> s |> Util.escape_html |> safe_val
      | _ -> raise Not_found)
  | "child_cnt" -> string_of_int_env "child_cnt" env
  | "comment" | "fnotes" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          get_comment fam |> get_note_source conf base m_auth conf.no_note
      | _ -> raise Not_found)
  | "count" -> (
      match get_env "count" env with
      | Vcnt c -> str_val (string_of_int !c)
      | _ -> null_val)
  | "count1" -> (
      match get_env "count1" env with
      | Vcnt c -> str_val (string_of_int !c)
      | _ -> null_val)
  | "count2" -> (
      match get_env "count2" env with
      | Vcnt c -> str_val (string_of_int !c)
      | _ -> null_val)
  | "count3" -> (
      match get_env "count3" env with
      | Vcnt c -> str_val (string_of_int !c)
      | _ -> null_val)
  | "desc_cnt" -> (
      match get_env "desc_cnt" env with
      | Vint c -> str_val (string_of_int c)
      | _ -> null_val)
  | "divorce_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env -> (
          match get_divorce fam with
          | Divorced d -> (
              match date_aux conf m_auth d with
              | VVstring s when s <> "" -> VVstring ("<em>" ^ s ^ "</em>")
              | x -> x)
          | NotDivorced | Separated -> raise Not_found)
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) -> (
              match get_divorce fam with
              | Divorced d -> (
                  match date_aux conf m_auth d with
                  | VVstring s when s <> "" -> VVstring ("<em>" ^ s ^ "</em>")
                  | x -> x)
              | NotDivorced | Separated -> raise Not_found)
          | _ -> raise Not_found))
  | "slash_divorce_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> (
          match get_divorce fam with
          | Divorced d -> (
              let d = Date.od_of_cdate d in
              match d with
              | Some d when m_auth ->
                  DateDisplay.string_slash_of_date conf d |> safe_val
              | _ -> null_val)
          | NotDivorced | Separated -> raise Not_found)
      | _ -> raise Not_found)
  | "empty_sorted_list" -> (
      match get_env "list" env with
      | Vslist l ->
          l := SortedList.empty;
          null_val
      | _ -> raise Not_found)
  | "empty_sorted_listb" -> (
      match get_env "listb" env with
      | Vslist l ->
          l := SortedList.empty;
          null_val
      | _ -> raise Not_found)
  | "empty_sorted_listc" -> (
      match get_env "listc" env with
      | Vslist l ->
          l := SortedList.empty;
          null_val
      | _ -> raise Not_found)
  | "empty_sorted_listd" -> (
      match get_env "listd" env with
      | Vslist l ->
          l := SortedList.empty;
          null_val
      | _ -> raise Not_found)
  | "empty_sorted_liste" -> (
      match get_env "liste" env with
      | Vslist l ->
          l := SortedList.empty;
          null_val
      | _ -> raise Not_found)
  | "family_cnt" -> (
      match get_env "family_cnt" env with
      | Vint x -> string_of_int x |> str_val
      | _ -> null_val)
  | "first_name_alias" -> (
      match get_env "first_name_alias" env with
      | Vstring s -> s |> Util.escape_html |> safe_val
      | _ -> null_val)
  | "fsources" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) ->
          get_fsources fam |> sou base |> Util.safe_html |> safe_val
      | _ -> null_val)
  | "url_in_env" -> (
      match get_env "url" env with Vstring x -> str_val x | _ -> str_val "")
  | "incr_count" -> (
      match get_env "count" env with
      | Vcnt c ->
          incr c;
          null_val
      | _ -> null_val)
  | "incr_count1" -> (
      match get_env "count1" env with
      | Vcnt c ->
          incr c;
          null_val
      | _ -> null_val)
  | "incr_count2" -> (
      match get_env "count2" env with
      | Vcnt c ->
          incr c;
          null_val
      | _ -> null_val)
  | "incr_count3" -> (
      match get_env "count3" env with
      | Vcnt c ->
          incr c;
          null_val
      | _ -> null_val)
  (* carrousel *)
  | "idigest" -> Image.default_portrait_filename base p |> str_val
  | "img_cnt" -> (
      match get_env "img_cnt" env with
      | Vint cnt -> VVstring (string_of_int cnt)
      | _ -> VVstring "")
  | "carrousel_img" -> (
      match get_env "carrousel_img" env with
      | Vstring s -> str_val s
      | _ -> null_val)
  | "carrousel_note" -> (
      match get_env "carrousel_note" env with
      | Vstring s -> str_val s
      | _ -> null_val)
  | "carrousel_src" -> (
      match get_env "carrousel_src" env with
      | Vstring s -> str_val s
      | _ -> null_val)
  (* end carrousel *)
  | "lazy_force" -> (
      match get_env "lazy_print" env with
      | Vlazyp r -> (
          match !r with
          | Some s ->
              r := None;
              safe_val (Adef.safe s)
          | None -> null_val)
      | _ -> raise Not_found)
  | "level" -> (
      match get_env "level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "list_size" -> (
      match get_env "list" env with
      | Vslist l -> str_val (string_of_int (SortedList.cardinal !l))
      | _ -> raise Not_found)
  | "listb_size" -> (
      match get_env "listb" env with
      | Vslist l -> str_val (string_of_int (SortedList.cardinal !l))
      | _ -> raise Not_found)
  | "listc_size" -> (
      match get_env "listc" env with
      | Vslist l -> str_val (string_of_int (SortedList.cardinal !l))
      | _ -> raise Not_found)
  | "listd_size" -> (
      match get_env "listd" env with
      | Vslist l -> str_val (string_of_int (SortedList.cardinal !l))
      | _ -> raise Not_found)
  | "liste_size" -> (
      match get_env "liste" env with
      | Vslist l -> str_val (string_of_int (SortedList.cardinal !l))
      | _ -> raise Not_found)
  | "marriage_place" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          if m_auth then
            get_marriage_place fam |> sou base |> Util.string_of_place conf
            |> safe_val
          else null_val
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) ->
              if m_auth then
                get_marriage_place fam |> sou base |> Util.string_of_place conf
                |> safe_val
              else null_val
          | _ -> raise Not_found))
  | "marriage_place_raw" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          if m_auth then
            get_marriage_place fam |> sou base
            |> Util.raw_string_of_place conf
            |> str_val
          else null_val
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) ->
              if m_auth then
                get_marriage_place fam |> sou base
                |> Util.raw_string_of_place conf
                |> str_val
              else null_val
          | _ -> raise Not_found))
  | "marriage_note" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          get_marriage_note fam |> get_note_source conf base m_auth conf.no_note
      | _ -> raise Not_found)
  | "marriage_source" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          get_marriage_src fam |> get_note_source conf base m_auth false
      | _ -> raise Not_found)
  | "max_anc_level" -> (
      match get_env "max_anc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "mode" -> (
      match get_env "mode" env with Vstring s -> str_val s | _ -> null_val)
  | "static_max_anc_level" -> (
      match get_env "static_max_anc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "sosa_ref_max_anc_level" -> (
      match get_env "sosa_ref_max_anc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "max_cous_level" -> (
      match get_env "max_cous_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "max_desc_level" -> (
      match get_env "max_desc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "nbr_a" -> (
      match get_env "nbr_a" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "nbr_a_l" -> (
      match get_env "nbr_a_l" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "static_max_desc_level" -> (
      match get_env "static_max_desc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "nobility_title" -> (
      match get_env "nobility_title" env with
      | Vtitle (p, t) ->
          if p_auth then
            string_of_title conf base (transl_nth conf "and" 0 |> Adef.safe) p t
            |> safe_val
          else null_val
      | _ -> raise Not_found)
  | "number_of_subitems" -> (
      match get_env "item" env with
      | Vslistlm ((s :: _) :: sll) ->
          let n =
            let rec loop n = function
              | (s1 :: _) :: sll -> if s = s1 then loop (n + 1) sll else n
              | _ -> n
            in
            loop 1 sll
          in
          str_val (string_of_int n)
      | _ -> raise Not_found)
  | "on_marriage_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          date_aux conf m_auth (get_marriage fam)
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) -> date_aux conf m_auth (get_marriage fam)
          | _ -> raise Not_found))
  | "slash_marriage_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> (
          match (m_auth, Date.od_of_cdate (get_marriage fam)) with
          | true, Some s -> DateDisplay.string_slash_of_date conf s |> safe_val
          | _ -> null_val)
      | _ -> raise Not_found)
  | "origin_file" ->
      if conf.wizard then
        match get_env "fam" env with
        | Vfam (_, fam, _, _) ->
            get_origin_file fam |> sou base |> Util.escape_html |> safe_val
        | _ -> null_val
      else raise Not_found
  | "qualifier" -> (
      match get_env "qualifier" env with
      | Vstring nn -> nn |> Util.escape_html |> safe_val
      | _ -> raise Not_found)
  | "related_type" -> (
      match get_env "rel" env with
      | Vrel (r, Some c) ->
          rchild_type_text conf r.r_type (index_of_sex (get_sex c)) |> safe_val
      | _ -> raise Not_found)
  | "relation_type" -> (
      match get_env "rel" env with
      | Vrel (r, None) -> (
          match (r.r_fath, r.r_moth) with
          | Some _, None -> relation_type_text conf r.r_type 0 |> safe_val
          | None, Some _ -> relation_type_text conf r.r_type 1 |> safe_val
          | Some _, Some _ -> relation_type_text conf r.r_type 2 |> safe_val
          | None, None -> raise Not_found)
      | _ -> raise Not_found)
  | "reset_count" -> (
      match get_env "count" env with
      | Vcnt c ->
          c := 0;
          null_val
      | _ -> null_val)
  | "reset_count1" -> (
      match get_env "count1" env with
      | Vcnt c ->
          c := 0;
          null_val
      | _ -> null_val)
  | "reset_count2" -> (
      match get_env "count2" env with
      | Vcnt c ->
          c := 0;
          null_val
      | _ -> null_val)
  | "reset_count3" -> (
      match get_env "count3" env with
      | Vcnt c ->
          c := 0;
          null_val
      | _ -> null_val)
  | "reset_desc_level" -> (
      let flevt_save =
        match get_env "desc_level_table_save" env with
        | Vdesclevtab levt ->
            let _, flevt = Lazy.force levt in
            flevt
        | _ -> raise Not_found
      in
      match get_env "desc_level_table" env with
      | Vdesclevtab levt ->
          let _, flevt = Lazy.force levt in
          Gwdb.Collection.iter
            (fun i -> Gwdb.Marker.set flevt i (Gwdb.Marker.get flevt_save i))
            (Gwdb.ifams base);
          null_val
      | _ -> raise Not_found)
  | "source_type" -> (
      match get_env "src_typ" env with
      | Vstring s -> s |> Util.safe_html |> safe_val
      | _ -> raise Not_found)
  | "surname_alias" -> (
      match get_env "surname_alias" env with
      | Vstring s -> s |> Util.safe_html |> safe_val
      | _ -> raise Not_found)
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then Util.escape_html v |> safe_val else raise Not_found

and eval_compound_var conf base env ((a, _) as ep) loc = function
  | "ancestor" :: sl -> (
      match get_env "ancestor" env with
      | Vanc gp -> eval_ancestor_field_var conf base env gp loc sl
      | Vanc_surn info -> eval_anc_by_surnl_field_var conf base env ep info sl
      | _ -> raise Not_found)
  | "descendant" :: sl -> (
      match get_env "descendant" env with
      | Vind p ->
          let ep = (p, authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "anc_paths_cnt_raw" :: sl ->
      eval_anc_paths_cnt conf base env ep Paths_cnt_raw false loc sl
  | "anc_paths_cnt" :: sl ->
      eval_anc_paths_cnt conf base env ep Paths_cnt false loc sl
  | "anc_paths" :: sl -> eval_anc_paths_cnt conf base env ep Paths false loc sl
  | "anc_paths_at_level_cnt_raw" :: sl ->
      eval_anc_paths_cnt conf base env ep Paths_cnt_raw true loc sl
  | "anc_paths_at_level_cnt" :: sl ->
      eval_anc_paths_cnt conf base env ep Paths_cnt true loc sl
  | "anc_paths_at_level" :: sl ->
      eval_anc_paths_cnt conf base env ep Paths true loc sl
  | "desc_paths_cnt_raw" :: sl ->
      eval_desc_paths_cnt conf base env ep Paths_cnt_raw false loc sl
  | "desc_paths_cnt" :: sl ->
      eval_desc_paths_cnt conf base env ep Paths_cnt false loc sl
  | "desc_paths" :: sl ->
      eval_desc_paths_cnt conf base env ep Paths false loc sl
  | "desc_paths_at_level_cnt_raw" :: sl ->
      eval_desc_paths_cnt conf base env ep Paths_cnt_raw true loc sl
  | "desc_paths_at_level_cnt" :: sl ->
      eval_desc_paths_cnt conf base env ep Paths_cnt true loc sl
  | "desc_paths_at_level" :: sl ->
      eval_desc_paths_cnt conf base env ep Paths true loc sl
  | ("baptism_witness" as s) :: sl
  | ("birth_witness" as s) :: sl
  | ("burial_witness" as s) :: sl
  | ("cremation_witness" as s) :: sl
  | ("death_witness" as s) :: sl
  | ("event_witness" as s) :: sl -> (
      match get_env s env with
      | Vind p ->
          let ep = (p, authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | [ "base"; "name" ] -> VVstring conf.bname
  | [ "plugin"; plugin ] ->
      VVbool (List.mem plugin (List.map Filename.basename conf.plugins))
  | "base" :: "nb_persons" :: sl ->
      VVstring (eval_int conf (nb_of_persons base) sl)
  | "base" :: "nb_families" :: sl ->
      VVstring (eval_int conf (nb_of_families base) sl)
  | "base" :: "real_nb_persons" :: sl ->
      VVstring (eval_int conf (Gwdb.nb_of_real_persons base) sl)
  | "cell" :: sl -> (
      match get_env "cell" env with
      | Vcell cell -> eval_cell_field_var conf base env cell loc sl
      | _ -> raise Not_found)
  | "child" :: sl -> (
      match get_env "child" env with
      | Vind p when mode_local env ->
          let auth = authorized_age conf base p in
          let ep = (p, auth) in
          eval_person_field_var conf base env ep loc sl
      | _ -> (
          match get_env "child_link" env with
          | Vind p ->
              let ep = (p, true) in
              let baseprefix =
                match get_env "baseprefix" env with
                | Vstring b -> b
                | _ -> conf.command
              in
              let conf = { conf with command = baseprefix } in
              eval_person_field_var conf base env ep loc sl
          | _ -> raise Not_found))
  | "cousin" :: sl -> (
      match get_env "cousin" env with
      | Vind p when mode_local env ->
          let auth = authorized_age conf base p in
          let ep = (p, auth) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "enclosing" :: sl ->
      let rec loop = function
        | ("#loop", _) :: env -> eval_person_field_var conf base env ep loc sl
        | _ :: env -> loop env
        | [] -> raise Not_found
      in
      loop env
  | "event_witness_relation" :: sl -> (
      match get_env "event_witness_relation" env with
      | Vevent (p, e) ->
          eval_event_witness_relation_var conf base env (p, e) loc sl
      | _ -> raise Not_found)
  | "event_witness_relation_kind" :: _ -> (
      match get_env "event_witness_relation_kind" env with
      | Vstring wk -> VVstring wk
      | _ -> raise Not_found)
  | "event_witness_kind" :: _ -> (
      match get_env "event_witness_kind" env with
      | Vstring s -> VVstring s
      | _ -> raise Not_found)
  | "witness_kind" :: _ -> (
      match get_env "witness_kind" env with
      | Vstring s -> VVstring s
      | _ -> raise Not_found)
  | "family" :: sl -> (
      (* TODO ???
         let mode_local =
         match get_env "fam_link" env with
         [ Vfam ifam _ (_, _, ip) _ -> False
         | _ -> True ]
         in *)
      match get_env "fam" env with
      | Vfam (i, f, c, m) ->
          eval_family_field_var conf base env (i, f, c, m) loc sl
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (i, f, c, m) ->
              eval_family_field_var conf base env (i, f, c, m) loc sl
          | _ -> raise Not_found))
  | "father" :: sl -> (
      match get_parents a with
      | Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match !GWPARAM_ITL.get_father conf base conf.command (get_iper a) with
          | Some (ep, base_prefix) ->
              let conf = { conf with command = base_prefix } in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None -> warning_use_has_parents_before_parent loc "father" null_val)
      )
  | "item" :: sl -> (
      match get_env "item" env with
      | Vslistlm ell -> eval_item_field_var ell sl
      | _ -> raise Not_found)
  | "mother" :: sl -> (
      match get_parents a with
      | Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match !GWPARAM_ITL.get_mother conf base conf.command (get_iper a) with
          | Some (ep, base_prefix) ->
              let conf = { conf with command = base_prefix } in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None -> warning_use_has_parents_before_parent loc "mother" null_val)
      )
  | "next_item" :: sl -> (
      match get_env "item" env with
      | Vslistlm (_ :: ell) -> eval_item_field_var ell sl
      | _ -> raise Not_found)
  | "nob_title" :: sl -> (
      match get_env "nob_title" env with
      | Vtitle (p, t) -> eval_title_field_var conf base env (p, t) loc sl
      | _ -> raise Not_found)
  | "number_of_ancestors" :: sl -> (
      match get_env "nbr_a" env with
      | Vint n -> VVstring (eval_int conf n sl)
      | _ -> raise Not_found)
  | "number_of_ancestors_at_level" :: sl | "nbr_anc_at_level" :: sl -> (
      match get_env "nbr_a_l" env with
      | Vint n -> VVstring (eval_int conf n sl)
      | _ -> raise Not_found)
  | "number_of_descendants" :: sl | "nbr_desc" :: sl ->
      number_of_descendants_aux conf base env true sl eval_int
  | "number_of_descendants_at_level" :: sl | "nbr_desc_at_level" :: sl ->
      number_of_descendants_aux conf base env false sl eval_int
  | "parent" :: sl -> (
      match get_env "parent" env with
      | Vind p ->
          let ep = (p, authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "path_end" :: sl -> (
      match get_env "path_end" env with
      | Vind p ->
          let auth = authorized_age conf base p in
          let ep = (p, auth) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | [ "person_index" ] -> (
      match find_person_in_env conf base "" with
      | Some p -> VVstring (Gwdb.string_of_iper (get_iper p))
      | None -> VVstring "")
  (* person_index.x -> i=, p=, n=, oc= *)
  (* person_index.1 -> i1=, p1=, n1=, oc1= *)
  (* person_index.2 -> i2=, p2=, n2=, oc2= *)
  (* person_index.e -> ei=, ep=, en=, eoc= *)
  | [ "person_index"; x ] -> (
      let find_person =
        match x with "e" -> find_person_in_env_pref | _ -> find_person_in_env
      in
      let s = if x = "x" then "" else x in
      match find_person conf base s with
      | Some p -> VVstring (Gwdb.string_of_iper (get_iper p))
      | None -> VVstring "")
  | "prev_item" :: sl -> (
      match get_env "prev_item" env with
      | Vslistlm ell -> eval_item_field_var ell sl
      | _ -> raise Not_found)
  | "prev_family" :: sl -> (
      match get_env "prev_fam" env with
      | Vfam (i, f, c, m) ->
          eval_family_field_var conf base env (i, f, c, m) loc sl
      | _ -> raise Not_found)
  | [ "prefix_new_ix"; ip ] ->
      let p =
        poi base (try iper_of_string ip with Failure _ -> raise Not_found)
      in
      str_val
        ((Util.commd ~excl:[ "iz"; "nz"; "pz"; "ocz" ] conf :> string)
        ^ "pz="
        ^ sou base (get_first_name p)
        ^ "&nz="
        ^ sou base (get_surname p)
        ^ (if get_occ p <> 0 then "&ocz=" ^ string_of_int (get_occ p) else "")
        ^ "&")
  | "pvar" :: v :: sl -> (
      match find_person_in_env conf base v with
      | Some p ->
          let ep = make_ep conf base (get_iper p) in
          eval_person_field_var conf base env ep loc sl
      | None -> raise Not_found)
  | "qvar" :: v :: sl ->
      (* %qvar.index_v.surname;
         direct access to a person whose index value is v
      *)
      let v1 = iper_of_string v in
      let v0 = int_of_string v in
      if v0 >= 0 && v0 < nb_of_persons base then
        let ep = make_ep conf base v1 in
        if is_hidden (fst ep) then raise Not_found
        else eval_person_field_var conf base env ep loc sl
      else raise Not_found
  | "p_of_index" :: v :: sl ->
      (* %p_of_index.index_v.surname;
         direct access to a person whose index value is v
      *)
      let i = int_of_string v in
      if i >= 0 && i < Gwdb.nb_of_persons base then
        let ip = iper_of_string v in
        let ep = make_ep conf base ip in
        if is_hidden (fst ep) then str_val ""
        else eval_person_field_var conf base env ep loc sl
      else raise Not_found
  | "f_of_index" :: v :: sl ->
      (* %f_of_index.index_v.marriage_date;
         direct access to a family whose index value is v
      *)
      let i = int_of_string v in
      if i >= 0 && i < Gwdb.nb_of_families base then
        let ifam = ifam_of_string v in
        let f, c, a = make_efam conf base (get_iper a) ifam in
        eval_family_field_var conf base env (ifam, f, c, a) loc sl
      else raise Not_found
  | [ "set_count"; n; v ] -> (
      match n with
      | "1" | "2" | "3" -> (
          match get_env ("count" ^ n) env with
          | Vcnt c ->
              c := int_of_string v;
              VVstring ""
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | [ "get_var"; name ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          (if not (List.mem name !GWPARAM.set_vars) then
           let name =
             if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
             else name
           in
           GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          let vv =
            try List.assoc name !lv with Not_found -> raise Not_found
          in
          VVstring vv
      | _ -> VVstring "")
  | [ "set_var"; name; value ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          if List.mem_assoc name !lv then lv := List.remove_assoc name !lv;
          lv := (name, value) :: !lv;
          (if not (List.mem name !GWPARAM.set_vars) then
           let name =
             if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
             else name
           in
           GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          VVstring ""
      | _ -> raise Not_found)
  | "svar" :: i :: sl -> (
      (* http://localhost:2317/HenriT_w?m=DAG&p1=henri&n1=duchmol&s1=243&s2=245
         access to sosa si=n of a person pi ni
         find_base_p will scan down starting from i such that multiple sosa of
         the same person can be listed
      *)
      let rec find_base_p j =
        let s = string_of_int j in
        let po = Util.find_person_in_env conf base s in
        match po with
        | Some p -> p
        | None -> if j = 0 then raise Not_found else find_base_p (j - 1)
      in
      let p0 = find_base_p (int_of_string i) in
      (* find sosa identified by si= of that person *)
      match p_getint conf.env ("s" ^ i) with
      | Some s -> (
          let s0 = Sosa.of_int s in
          let ip0 = get_iper p0 in
          match Util.branch_of_sosa conf base s0 (pget conf base ip0) with
          | Some (p :: _) ->
              let p_auth = authorized_age conf base p in
              eval_person_field_var conf base env (p, p_auth) loc sl
          | _ -> raise Not_found)
      | None -> raise Not_found)
  | "sosa_anc" :: s :: sl -> (
      (* %sosa_anc.sosa.first_name;
         direct access to a person whose sosa relative to sosa_ref is s
      *)
      match get_env "sosa_ref" env with
      | Vsosa_ref (Some p) -> (
          let ip = get_iper p in
          let s0 = Sosa.of_string s in
          match Util.branch_of_sosa conf base s0 (pget conf base ip) with
          | Some (p :: _) ->
              let p_auth = authorized_age conf base p in
              eval_person_field_var conf base env (p, p_auth) loc sl
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | "sosa_anc_p" :: s :: sl -> (
      (* %sosa_anc_p.sosa.first_name;
         direct access to a person whose sosa relative to current person
      *)
      match Util.p_of_sosa conf base (Sosa.of_string s) a with
      | Some np ->
          let np_auth = authorized_age conf base np in
          eval_person_field_var conf base env (np, np_auth) loc sl
      | None -> raise Not_found)
  | "related" :: sl -> (
      match get_env "rel" env with
      | Vrel ({ r_type = rt }, Some p) ->
          eval_relation_field_var conf base env
            (index_of_sex (get_sex p), rt, get_iper p, false)
            loc sl
      | _ -> raise Not_found)
  | "relation_her" :: sl -> (
      match get_env "rel" env with
      | Vrel ({ r_moth = Some ip; r_type = rt }, None) ->
          eval_relation_field_var conf base env (1, rt, ip, true) loc sl
      | _ -> raise Not_found)
  | "relation_him" :: sl -> (
      match get_env "rel" env with
      | Vrel ({ r_fath = Some ip; r_type = rt }, None) ->
          eval_relation_field_var conf base env (0, rt, ip, true) loc sl
      | _ -> raise Not_found)
  | "self" :: sl -> eval_person_field_var conf base env ep loc sl
  | "sosa_ref" :: sl -> (
      match get_env "sosa_ref" env with
      | Vsosa_ref (Some p) ->
          let ep = make_ep conf base (get_iper p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "spouse" :: sl -> (
      match get_env "fam" env with
      | Vfam (_, _, (_, _, ip), _) when mode_local env ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, _, (_, _, ip), _) -> (
              let baseprefix =
                match get_env "baseprefix" env with
                | Vstring baseprefix -> baseprefix
                | _ -> conf.command
              in
              match !GWPARAM_ITL.get_person conf base baseprefix ip with
              | Some (ep, baseprefix) ->
                  let conf = { conf with command = baseprefix } in
                  let env = ("p_link", Vbool true) :: env in
                  eval_person_field_var conf base env ep loc sl
              | None -> raise Not_found)
          | _ -> raise Not_found))
  | "witness" :: sl -> (
      match get_env "witness" env with
      | Vind p ->
          let ep = (p, authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "witness_relation" :: sl -> (
      match get_env "fam" env with
      | Vfam (i, f, c, m) ->
          eval_witness_relation_var conf base env (i, f, c, m) loc sl
      | _ -> raise Not_found)
  | sl -> eval_person_field_var conf base env ep loc sl

and eval_anc_paths_cnt conf base env (p, _) path_mode at_to ?(l1_l2 = (0, 0))
    _loc = function
  | sl -> (
      match get_env "level" env with
      | Vint lev -> (
          match path_mode with
          | Paths_cnt_raw -> (
              let list1 = Cousins.anc_cnt_aux conf base lev at_to p in
              match list1 with
              | Some list1 -> VVstring (eval_int conf (List.length list1) sl)
              | None -> raise Not_found)
          | Paths_cnt -> (
              let list1 = Cousins.anc_cnt_aux conf base lev at_to p in
              match list1 with
              | Some list1 ->
                  VVstring
                    (eval_int conf
                       (List.length (Cousins.cousins_fold list1))
                       sl)
              | None -> raise Not_found)
          | Paths -> (
              let l = Cousins.anc_cnt_aux conf base lev at_to p in
              match l with
              | Some l -> (
                  match get_env "cousins" env with
                  | Vcousl cl ->
                      cl := Cousins.cousins_fold l;
                      let l1, l2 = l1_l2 in
                      (match get_env "v1_v2" env with
                      | Vcous_level (v1, v2) ->
                          v1 := l1;
                          v2 := l2
                      | _ -> ());
                      VVstring ""
                  | _ -> raise Not_found)
              | None -> raise Not_found))
      | _ -> raise Not_found)

and eval_desc_paths_cnt conf base env (p, _) path_mode at_to ?(l1_l2 = (0, 0))
    _loc = function
  | sl -> (
      match get_env "level" env with
      | Vint lev -> (
          match path_mode with
          | Paths_cnt_raw -> (
              let list1 = Cousins.desc_cnt_aux conf base lev at_to p in
              match list1 with
              | Some list1 -> VVstring (eval_int conf (List.length list1) sl)
              | None -> raise Not_found)
          | Paths_cnt -> (
              let list1 = Cousins.desc_cnt_aux conf base lev at_to p in
              match list1 with
              | Some l ->
                  VVstring
                    (eval_int conf (List.length (Cousins.cousins_fold l)) sl)
              | None -> raise Not_found)
          | Paths -> (
              let l = Cousins.desc_cnt_aux conf base lev at_to p in
              match l with
              | Some l -> (
                  match get_env "cousins" env with
                  | Vcousl cl ->
                      cl := Cousins.cousins_fold l;
                      let l1, l2 = l1_l2 in
                      (match get_env "v1_v2" env with
                      | Vcous_level (v1, v2) ->
                          v1 := l1;
                          v2 := l2
                      | _ -> ());
                      VVstring ""
                  | _ -> VVstring "")
              | None -> (
                  match get_env "cousins" env with
                  | Vcousl cl ->
                      cl := [];
                      (match get_env "v1_v2" env with
                      | Vcous_level (v1, v2) ->
                          v1 := 0;
                          v2 := 0
                      | _ -> ());
                      VVstring ""
                  | _ -> VVstring "")))
      | _ -> raise Not_found)

and eval_item_field_var ell = function
  | [ s ] -> (
      try
        match ell with
        | el :: _ ->
            let v = int_of_string s in
            let r = try List.nth el (v - 1) with Failure _ -> "" in
            VVstring r
        | [] -> null_val
      with Failure _ -> raise Not_found)
  | _ -> raise Not_found

and eval_title_field_var conf base env (_p, (nth, name, title, places, dates))
    _loc = function
  | [ "is_first" ] ->
      VVbool
        (match get_env "first" env with Vbool x -> x | _ -> raise Not_found)
  | [ "is_main" ] -> (
      match name with
      | Tmain -> bool_val true
      | _ -> (
          match get_env "first" env with
          | Vbool x -> bool_val x
          | _ -> bool_val false))
  | [ "nth" ] -> VVstring (string_of_int nth)
  | [ "name" ] -> (
      match name with
      | Tname n -> VVstring (sou base n |> escape_html :> string)
      | _ -> VVstring "")
  | [ "title" ] -> VVstring (sou base title |> escape_html :> string)
  | [ "places" ] ->
      let places =
        List.map (fun pl -> (sou base pl |> escape_html :> string)) places
      in
      VVstring (String.concat ", " places)
  | [ "dates" ] ->
      let date_opt_to_string d =
        match d with
        | Some (Dgreg (dmy, _)) ->
            Some (DateDisplay.string_of_dmy conf dmy :> string)
        | Some (Dtext d) -> Some (d |> escape_html :> string)
        | None -> None
      in
      let dates =
        List.map
          (fun (d1, d2) ->
            match (date_opt_to_string d1, date_opt_to_string d2) with
            | Some s1, Some s2 -> Format.sprintf "%s - %s" s1 s2
            | Some s1, None -> Format.sprintf "%s -" s1
            | None, Some s2 -> Format.sprintf "- %s" s2
            | None, None -> "")
          dates
      in
      VVstring (String.concat ", " dates)
  | [ "date_begin" ] -> (
      match dates with
      | [ (d, _) ] -> (
          match d with
          | Some (Dgreg (dmy, _)) ->
              VVstring (DateDisplay.string_of_dmy conf dmy :> string)
          | Some (Dtext d) -> VVstring (d |> escape_html :> string)
          | None -> null_val)
      | _ -> VVstring "multiple dates")
  | [ "date_end" ] -> (
      match dates with
      | [ (_, d) ] -> (
          match d with
          | Some (Dgreg (dmy, _)) ->
              VVstring (DateDisplay.string_of_dmy conf dmy :> string)
          | Some (Dtext d) -> VVstring (d |> escape_html :> string)
          | None -> null_val)
      | _ -> VVstring "multiple dates")
  | _ -> raise Not_found

and eval_relation_field_var conf base env (i, rt, ip, is_relation) loc =
  function
  | [ "type" ] ->
      if is_relation then safe_val (relation_type_text conf rt i)
      else safe_val (rchild_type_text conf rt i)
  | sl ->
      let ep = make_ep conf base ip in
      eval_person_field_var conf base env ep loc sl

and eval_cell_field_var conf base env cell loc = function
  | [ "colspan" ] -> (
      match cell with
      | Empty -> VVstring "1"
      | Cell (_, _, _, _, s, _) -> VVstring (string_of_int s))
  | "family" :: sl -> (
      match cell with
      | Cell (p, Some ifam, _, _, _, base_prefix) -> (
          if conf.bname = base_prefix then
            let f, c, a = make_efam conf base (get_iper p) ifam in
            eval_family_field_var conf base env (ifam, f, c, a) loc sl
          else
            let conf = { conf with command = base_prefix } in
            match !GWPARAM_ITL.get_family conf base base_prefix p ifam with
            | Some (f, c, a) ->
                eval_family_field_var conf base env (ifam, f, c, a) loc sl
            | None -> assert false)
      | _ -> VVstring "")
  | [ "is_center" ] -> (
      match cell with
      | Cell (_, _, Center, _, _, _) -> VVbool true
      | _ -> VVbool false)
  | [ "is_empty" ] -> (
      match cell with Empty -> VVbool true | _ -> VVbool false)
  | [ "is_left" ] -> (
      match cell with
      | Cell (_, _, Left, _, _, _) -> VVbool true
      | _ -> VVbool false)
  | [ "is_right" ] -> (
      match cell with
      | Cell (_, _, Right, _, _, _) -> VVbool true
      | _ -> VVbool false)
  | [ "is_top" ] -> (
      match cell with
      | Cell (_, _, _, false, _, _) -> VVbool true
      | _ -> VVbool false)
  | "person" :: sl -> (
      match cell with
      | Cell (p, _, _, _, _, base_prefix) ->
          if conf.bname = base_prefix then
            let ep = make_ep conf base (get_iper p) in
            eval_person_field_var conf base env ep loc sl
          else
            let conf = { conf with command = base_prefix } in
            let ep = (p, true) in
            eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | _ -> raise Not_found

and eval_ancestor_field_var conf base env gp loc = function
  | "family" :: sl -> (
      match gp with
      | GP_person (_, ip, Some ifam) ->
          let f = foi base ifam in
          let ifath = get_father f in
          let imoth = get_mother f in
          let ispouse = if ip = ifath then imoth else ifath in
          let c = (ifath, imoth, ispouse) in
          let m_auth =
            authorized_age conf base (pget conf base ifath)
            && authorized_age conf base (pget conf base imoth)
          in
          eval_family_field_var conf base env (ifam, f, c, m_auth) loc sl
      | _ -> raise Not_found)
  | "father" :: sl -> (
      match gp with
      | GP_person (_, ip, _) -> (
          match (get_parents (pget conf base ip), get_env "all_gp" env) with
          | Some ifam, Vallgp all_gp -> (
              let cpl = foi base ifam in
              match get_link all_gp (get_father cpl) with
              | Some gp -> eval_ancestor_field_var conf base env gp loc sl
              | None ->
                  let ep = make_ep conf base (get_father cpl) in
                  eval_person_field_var conf base env ep loc sl)
          | _, _ -> raise Not_found)
      | GP_same (_, _, ip) -> (
          match get_parents (pget conf base ip) with
          | Some ifam ->
              let cpl = foi base ifam in
              let ep = make_ep conf base (get_father cpl) in
              eval_person_field_var conf base env ep loc sl
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | [ "father_sosa" ] -> (
      match (gp, get_env "all_gp" env) with
      | (GP_person (n, ip, _) | GP_same (n, _, ip)), Vallgp all_gp ->
          let n = Sosa.twice n in
          VVstring (parent_sosa conf base ip all_gp n get_father)
      | _ -> null_val)
  | "interval" :: sl -> (
      let to_string x = eval_sosa conf x sl in
      match gp with
      | GP_interv (Some (n1, n2, Some (n3, n4))) ->
          let n2 = Sosa.sub n2 Sosa.one in
          let n4 = Sosa.sub n4 Sosa.one in
          VVstring
            (to_string n1 ^ "-" ^ to_string n2 ^ " = " ^ to_string n3 ^ "-"
           ^ to_string n4)
      | GP_interv (Some (n1, n2, None)) ->
          let n2 = Sosa.sub n2 Sosa.one in
          VVstring (to_string n1 ^ "-" ^ to_string n2 ^ " = ...")
      | GP_interv None -> VVstring "..."
      | GP_person _ | GP_same _ | GP_missing _ -> null_val)
  | [ "mother_sosa" ] -> (
      match (gp, get_env "all_gp" env) with
      | (GP_person (n, ip, _) | GP_same (n, _, ip)), Vallgp all_gp ->
          let n = Sosa.inc (Sosa.twice n) 1 in
          VVstring (parent_sosa conf base ip all_gp n get_mother)
      | _ -> null_val)
  | "same" :: sl -> (
      match gp with
      | GP_same (_, n, _) -> VVstring (eval_sosa conf n sl)
      | GP_person _ | GP_interv _ | GP_missing _ -> null_val)
  | "anc_sosa" :: sl -> (
      match gp with
      | GP_person (n, _, _) | GP_same (n, _, _) ->
          VVstring (eval_sosa conf n sl)
      | GP_interv _ | GP_missing _ -> null_val)
  | "spouse" :: sl -> (
      match gp with
      | GP_person (_, ip, Some ifam) ->
          let ip = Gutil.spouse ip (foi base ifam) in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | GP_person _ | GP_interv _ | GP_missing _ | GP_same _ -> raise Not_found)
  | sl -> (
      match gp with
      | GP_person (_, ip, _) | GP_same (_, _, ip) ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | GP_interv _ | GP_missing _ -> raise Not_found)

and eval_anc_by_surnl_field_var conf base env ep info =
  match info with
  | Branch (_, db, de, place, p, sosa_list, loc) -> (
      function
      | "date_begin" :: sl -> (
          match db with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | "date_end" :: sl -> (
          match de with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | [ "nb_times" ] -> str_val (string_of_int (List.length sosa_list))
      | [ "place" ] -> safe_val (Util.string_of_place conf place)
      | [ "sosa_access" ] ->
          let str, _ =
            List.fold_right
              (fun sosa (str, n) ->
                ( str ^^^ "&s" ^<^ string_of_int n ^<^ "="
                  ^<^ (Sosa.to_string sosa |> Mutil.encode),
                  n + 1 ))
              sosa_list
              (Adef.encoded "", 1)
          in
          let p, _ = ep in
          safe_val
            ((acces_n conf base (Adef.escaped "1") p
               : Adef.escaped_string
               :> Adef.safe_string)
            ^^^ (str : Adef.encoded_string :> Adef.safe_string))
      | sl ->
          let ep = make_ep conf base (get_iper p) in
          eval_person_field_var conf base env ep loc sl)
  | Eclair (_, place, db, de, p, persl, loc) -> (
      function
      | "date_begin" :: sl -> (
          match db with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | "date_end" :: sl -> (
          match de with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | [ "nb_events" ] -> VVstring (string_of_int (List.length persl))
      | [ "nb_ind" ] ->
          IperSet.elements (List.fold_right IperSet.add persl IperSet.empty)
          |> List.length |> string_of_int |> str_val
      | [ "place" ] -> safe_val place
      | sl ->
          let ep = make_ep conf base (get_iper p) in
          eval_person_field_var conf base env ep loc sl)

and eval_sosa conf n = function
  | [ "hexa" ] -> Printf.sprintf "0x%X" @@ int_of_string (Sosa.to_string n)
  | [ "octal" ] -> Printf.sprintf "0x%o" @@ int_of_string (Sosa.to_string n)
  | [ "lvl" ] -> string_of_int @@ Sosa.gen n
  | [ "v" ] -> Sosa.to_string n
  | [] -> Sosa.to_string_sep (transl conf "(thousand separator)") n
  | _ -> raise Not_found

and eval_int conf n = function
  | [ "hexa" ] -> Printf.sprintf "0x%X" n
  | [ "octal" ] -> Printf.sprintf "0x%o" n
  | [ "v" ] -> string_of_int n
  | [] -> Mutil.string_of_int_sep (transl conf "(thousand separator)") n
  | _ -> raise Not_found

and eval_person_field_var conf base env ((p, p_auth) as ep) loc = function
  | "anc1" :: sl -> (
      match get_env "anc1" env with
      | Vind pa ->
          eval_person_field_var conf base env
            (pa, authorized_age conf base pa)
            loc sl
      | _ -> VVstring "")
  | "anc2" :: sl -> (
      match get_env "anc2" env with
      | Vind pa ->
          eval_person_field_var conf base env
            (pa, authorized_age conf base pa)
            loc sl
      | _ -> VVstring "")
  | [ "anc_f_list" ] -> (
      match get_env "anc_f_list" env with
      | Vstring ifaml -> VVstring ifaml
      | _ -> VVstring "")
  | [ "anc_level" ] -> (
      match get_env "anc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> raise Not_found)
  | "baptism_date" :: sl -> (
      match Date.od_of_cdate (get_baptism p) with
      | Some d when p_auth -> eval_date_field_var conf d sl
      | Some _ | None -> null_val)
  | "birth_date" :: sl -> (
      match Date.od_of_cdate (get_birth p) with
      | Some d when p_auth -> eval_date_field_var conf d sl
      | Some _ | None -> null_val)
  | "burial_date" :: sl -> (
      match get_burial p with
      | Buried cod when p_auth -> (
          match Date.od_of_cdate cod with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | Buried _ | Cremated _ | UnknownBurial -> null_val)
  | [ "cnt" ] -> (
      match get_env "cnt" env with
      | Vint cnt -> VVstring (string_of_int cnt)
      | _ -> VVstring "")
  | [ "cous_paths_min_date"; l1; l2 ] -> (
      match Cousins.min_max_date conf base p true l1 l2 with
      | Some min -> VVstring (string_of_int min)
      | None -> raise Not_found)
  | [ "cous_paths_max_date"; l1; l2 ] -> (
      match Cousins.min_max_date conf base p false l1 l2 with
      | Some max -> VVstring (string_of_int max)
      | None -> raise Not_found)
  | [ "cous_paths_cnt_raw"; l1; l2 ] -> (
      let l = Cousins.cousins_l1_l2_aux conf base l1 l2 p in
      match l with
      | Some l -> VVstring (string_of_int (List.length l))
      | None -> VVstring "-1")
  | [ "cous_paths_cnt"; l1; l2 ] -> (
      let l = Cousins.cousins_l1_l2_aux conf base l1 l2 p in
      match l with
      | Some l ->
          VVstring (string_of_int (List.length (Cousins.cousins_fold l)))
      | None -> VVstring "-1")
  | [ "cous_paths"; l1; l2 ] -> (
      let l = Cousins.cousins_l1_l2_aux conf base l1 l2 p in
      match l with
      | Some l -> (
          match get_env "cousins" env with
          | Vcousl cl ->
              cl := Cousins.cousins_fold l;
              null_val
          | _ -> raise Not_found)
      | None -> VVstring "-1")
  | [ "cous_implx_cnt"; l1; l2 ] -> (
      match p_getenv conf.env "c_implex" with
      | Some "on" | Some "1" ->
          let cnt = Cousins.cousins_implex_cnt conf base l1 l2 p in
          VVstring (string_of_int cnt)
      | _ -> VVstring "")
  | [ "cousins"; "max_a" ] ->
      let max_a, _ = Cousins.max_l1_l2 conf base p in
      VVstring (string_of_int max_a)
  | [ "cousins"; "max_d" ] ->
      let _, max_d = Cousins.max_l1_l2 conf base p in
      VVstring (string_of_int max_d)
  | [ "cousins_cnt"; l1; l2 ] -> (
      let l = Cousins.cousins_l1_l2_aux conf base l1 l2 p in
      match l with
      | Some l ->
          let l =
            List.map (fun (ip, _, _, _) -> ip) l |> List.sort_uniq compare
          in
          VVstring (string_of_int (List.length l))
      | None -> VVstring "-1")
  | "cremated_date" :: sl -> (
      match get_burial p with
      | Cremated cod when p_auth -> (
          match Date.od_of_cdate cod with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | Buried _ | Cremated _ | UnknownBurial -> null_val)
  | "death_date" :: sl -> (
      match get_death p with
      | Death (_, cd) when p_auth ->
          eval_date_field_var conf (Date.date_of_cdate cd) sl
      | Death _ | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead
      | OfCourseDead ->
          null_val)
  | "event" :: sl -> (
      match get_env "event" env with
      | Vevent (_, e) -> eval_event_field_var conf base env ep e loc sl
      | _ -> raise Not_found)
  | "father" :: sl -> (
      match get_parents p with
      | Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match !GWPARAM_ITL.get_father conf base conf.command (get_iper p) with
          | Some (ep, baseprefix) ->
              let conf = { conf with command = baseprefix } in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None -> warning_use_has_parents_before_parent loc "father" null_val)
      )
  | [ "has_linked_page"; s ] -> (
      match get_env "nldb" env with
      | Vnldb db ->
          let key =
            let fn = Name.lower (sou base (get_first_name p)) in
            let sn = Name.lower (sou base (get_surname p)) in
            (fn, sn, get_occ p)
          in
          let r =
            List.exists
              (fun (pg, (_, il)) ->
                match pg with
                | Def.NLDB.PgMisc pg ->
                    if List.mem_assoc key il then
                      let nenv, _ = Notes.read_notes base pg in
                      List.mem_assoc s nenv
                    else false
                | _ -> false)
              db
          in
          VVbool r
      | _ -> raise Not_found)
  | [ "has_linked_pages" ] -> (
      match get_env "nldb" env with
      | Vnldb db ->
          let r =
            if p_auth then
              let key =
                let fn = Name.lower (sou base (get_first_name p)) in
                let sn = Name.lower (sou base (get_surname p)) in
                (fn, sn, get_occ p)
              in
              links_to_ind conf base db key <> []
            else false
          in
          VVbool r
      | _ -> raise Not_found)
  | [ "has_sosa" ] -> (
      match get_env "p_link" env with
      | Vbool _ -> VVbool false
      | _ -> (
          match get_env "sosa" env with
          | Vsosa r -> VVbool (get_sosa conf base env r p <> None)
          | _ -> VVbool false))
  | [ "init_cache"; nb_asc; from_gen_desc; nb_desc ] -> (
      try
        let nb_asc = int_of_string nb_asc in
        let from_gen_desc = int_of_string from_gen_desc in
        let nb_desc = int_of_string nb_desc in
        let () =
          !GWPARAM_ITL.init_cache conf base (get_iper p) nb_asc from_gen_desc
            nb_desc
        in
        null_val
      with _ -> raise Not_found)
  | [ "lev_cnt" ] -> (
      match get_env "lev_cnt" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> raise Not_found)
  | [ "linked_page"; s ] -> (
      match get_env "nldb" env with
      | Vnldb db ->
          let key =
            let fn = Name.lower (sou base (get_first_name p)) in
            let sn = Name.lower (sou base (get_surname p)) in
            (fn, sn, get_occ p)
          in
          List.fold_left (linked_page_text conf base p s key) (Adef.safe "") db
          |> safe_val
      | _ -> raise Not_found)
  | "marriage_date" :: sl -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, true) -> (
          match Date.od_of_cdate (get_marriage fam) with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | _ -> raise Not_found)
  | "mother" :: sl -> (
      match get_parents p with
      | Some ifam ->
          let cpl = foi base ifam in
          let ep = make_ep conf base (get_mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match !GWPARAM_ITL.get_mother conf base conf.command (get_iper p) with
          | Some (ep, baseprefix) ->
              let conf = { conf with command = baseprefix } in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None -> warning_use_has_parents_before_parent loc "mother" null_val)
      )
  | [ "nbr" ] -> (
      match get_env "nbr" env with
      | Vint nbr -> VVstring (string_of_int nbr)
      | _ -> VVstring "")
  | "nobility_title" :: sl -> (
      match Util.main_title conf base p with
      | Some t when p_auth ->
          let id = sou base t.t_ident in
          let pl = sou base t.t_place in
          eval_nobility_title_field_var (id, pl) sl
      | Some _ | None -> null_val)
  | "self" :: sl -> eval_person_field_var conf base env ep loc sl
  | "sosa" :: sl -> (
      match get_env "sosa" env with
      | Vsosa x -> (
          match get_sosa conf base env x p with
          | Some (n, _) -> VVstring (eval_sosa conf n sl)
          | None -> null_val)
      | _ -> raise Not_found)
  | "sosa_next" :: sl -> (
      match get_env "sosa" env with
      | Vsosa x -> (
          match get_sosa conf base env x p with
          | Some (n, _) -> (
              match SosaCache.next_sosa n with
              | so, ip ->
                  if so = Sosa.zero then null_val
                  else
                    let p = poi base ip in
                    let p_auth = authorized_age conf base p in
                    eval_person_field_var conf base env (p, p_auth) loc sl)
          | None -> null_val)
      | _ -> raise Not_found)
  | "sosa_prev" :: sl -> (
      match get_env "sosa" env with
      | Vsosa x -> (
          match get_sosa conf base env x p with
          | Some (n, _) -> (
              match SosaCache.prev_sosa n with
              | so, ip ->
                  if Sosa.eq so Sosa.zero then null_val
                  else
                    let p = poi base ip in
                    let p_auth = authorized_age conf base p in
                    eval_person_field_var conf base env (p, p_auth) loc sl)
          | None -> null_val)
      | _ -> raise Not_found)
  | "spouse" :: sl -> (
      match get_env "fam" env with
      | Vfam (ifam, _, _, _) ->
          let cpl = foi base ifam in
          let ip = Gutil.spouse (get_iper p) cpl in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | [ "var" ] -> VVother (eval_person_field_var conf base env ep loc)
  | [ s ] -> (
      try bool_val (eval_bool_person_field conf base env ep s)
      with Not_found -> eval_str_person_field conf base env ep s)
  | [] -> simple_person_text conf base p p_auth |> safe_val
  | _ -> raise Not_found

and eval_date_field_var conf d = function
  | [ "prec" ] -> (
      match d with
      | Dgreg (dmy, _) ->
          DateDisplay.prec_text conf dmy |> Util.escape_html |> safe_val
      | _ -> null_val)
  | [ "day" ] -> (
      match d with
      | Dgreg (dmy, _) ->
          if dmy.day = 0 then null_val else VVstring (string_of_int dmy.day)
      | _ -> null_val)
  | [ "day2" ] -> (
      match d with
      | Dgreg (dmy, _) -> (
          match dmy.prec with
          | OrYear dmy2 | YearInt dmy2 ->
              if dmy2.day2 = 0 then null_val
              else VVstring (string_of_int dmy2.day2)
          | _ -> null_val)
      | _ -> null_val)
  | [ "julian_day" ] -> (
      match d with
      | Dgreg (dmy, _) ->
          VVstring (string_of_int (Calendar.sdn_of_gregorian dmy))
      | _ -> null_val)
  | [ "month" ] -> (
      match d with
      | Dgreg (dmy, _) -> VVstring (DateDisplay.month_text dmy)
      | _ -> null_val)
  | [ "month2" ] -> (
      match d with
      | Dgreg (dmy, _) -> (
          match dmy.prec with
          | OrYear dmy2 | YearInt dmy2 ->
              if dmy2.month2 = 0 then null_val
              else VVstring (string_of_int dmy2.month2)
          | _ -> null_val)
      | _ -> null_val)
  | [ "year" ] -> (
      match d with
      | Dgreg (dmy, _) -> VVstring (string_of_int dmy.year)
      | _ -> null_val)
  | [ "year2" ] -> (
      match d with
      | Dgreg (dmy, _) -> (
          match dmy.prec with
          | OrYear dmy2 | YearInt dmy2 -> VVstring (string_of_int dmy2.year2)
          | _ -> null_val)
      | _ -> null_val)
  | [] ->
      DateDisplay.string_of_date_aux ~link:false conf
        ~sep:(Adef.safe "&#010;  ") d
      |> safe_val
  | _ -> raise Not_found

and _eval_place_field_var conf place = function
  | [] ->
      (* Compatibility before eval_place_field_var *)
      VVstring place
  | [ "other" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.other
      | None -> null_val)
  | [ "town" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.town
      | None -> null_val)
  | [ "township" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.township
      | None -> null_val)
  | [ "canton" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.canton
      | None -> null_val)
  | [ "district" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.district
      | None -> null_val)
  | [ "county" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.county
      | None -> null_val)
  | [ "region" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.region
      | None -> null_val)
  | [ "country" ] -> (
      match place_of_string conf place with
      | Some p -> VVstring p.country
      | None -> null_val)
  | _ -> raise Not_found

and eval_nobility_title_field_var (id, pl) = function
  | [ "ident_key" ] -> safe_val (Mutil.encode id)
  | [ "place_key" ] -> safe_val (Mutil.encode pl)
  | [] -> VVstring (if pl = "" then id else id ^ " " ^ pl)
  | _ -> raise Not_found

and eval_bool_event_field base (p, p_auth) (_, date, place, note, src, w, isp) =
  function
  | "has_date" -> p_auth && date <> Date.cdate_None
  | "has_place" -> p_auth && sou base place <> ""
  | "has_note" -> p_auth && sou base note <> ""
  | "has_src" -> p_auth && sou base src <> ""
  | "has_witnesses" -> p_auth && Array.length w > 0
  | "has_spouse" -> p_auth && isp <> None
  | "computable_age" ->
      if p_auth then
        match Date.cdate_to_dmy_opt (get_birth p) with
        | Some d -> not (d.day = 0 && d.month = 0 && d.prec <> Sure)
        | None -> (
            match Date.cdate_to_dmy_opt (get_baptism p) with
            | Some d -> not (d.day = 0 && d.month = 0 && d.prec <> Sure)
            | None -> false)
      else false
  | _ -> raise Not_found

and eval_str_event_field conf base (p, p_auth)
    (name, date, place, note, src, _, _) = function
  | "age" ->
      if p_auth then
        let birth_date, approx =
          match Date.cdate_to_dmy_opt (get_birth p) with
          | None -> (Date.cdate_to_dmy_opt (get_baptism p), true)
          | x -> (x, false)
        in
        match (birth_date, Date.cdate_to_dmy_opt date) with
        | ( Some ({ prec = Sure | About | Maybe } as d1),
            Some ({ prec = Sure | About | Maybe } as d2) )
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            let s =
              if (not approx) && d1.prec = Sure && d2.prec = Sure then ""
              else transl_decline conf "possibly (date)" "" ^ " "
            in
            safe_val (s ^<^ DateDisplay.string_of_age conf a)
        | _ -> null_val
      else null_val
  | "name" -> (
      if not p_auth then null_val
      else
        match name with
        | Event.Pevent name ->
            Util.string_of_pevent_name conf base name |> safe_val
        | Event.Fevent name ->
            Util.string_of_fevent_name conf base name |> safe_val)
  | "date" -> (
      if not p_auth then null_val
      else
        match Date.od_of_cdate date with
        | Some d -> DateDisplay.string_of_date conf d |> safe_val
        | None -> null_val)
  | "on_date" -> date_aux conf p_auth date
  | "place" ->
      if p_auth then sou base place |> Util.string_of_place conf |> safe_val
      else null_val
  | "note" -> note |> get_note_source conf base ~p p_auth conf.no_note
  | "src" -> src |> get_note_source conf base ~p p_auth false
  | _ -> raise Not_found

and eval_event_field_var conf base env (p, p_auth)
    (name, date, place, note, src, w, isp) loc = function
  | "date" :: sl -> (
      match (p_auth, Date.od_of_cdate date) with
      | true, Some d -> eval_date_field_var conf d sl
      | _ -> null_val)
  | "spouse" :: sl -> (
      match isp with
      | Some isp ->
          let sp = poi base isp in
          let ep = (sp, authorized_age conf base sp) in
          eval_person_field_var conf base env ep loc sl
      | None -> null_val)
  | [ s ] -> (
      try
        bool_val
          (eval_bool_event_field base (p, p_auth)
             (name, date, place, note, src, w, isp)
             s)
      with Not_found ->
        eval_str_event_field conf base (p, p_auth)
          (name, date, place, note, src, w, isp)
          s)
  | _ -> raise Not_found

and eval_event_witness_relation_var conf base env (p, e) loc = function
  | "event" :: sl ->
      let ep = (p, authorized_age conf base p) in
      eval_event_field_var conf base env ep e loc sl
  | "person" :: sl ->
      let ep = (p, authorized_age conf base p) in
      eval_person_field_var conf base env ep loc sl
  | _ -> raise Not_found

and eval_bool_person_field conf base env (p, p_auth) = function
  | "access_by_key" ->
      Util.accessible_by_key conf base p (p_first_name base p)
        (p_surname base p)
  | "birthday" -> (
      match (p_auth, Date.cdate_to_dmy_opt (get_birth p)) with
      | true, Some d ->
          if d.prec = Sure && get_death p = NotDead then
            d.day = conf.today.day && d.month = conf.today.month
            && d.year < conf.today.year
            || (not (Date.leap_year conf.today.year))
               && d.day = 29 && d.month = 2 && conf.today.day = 1
               && conf.today.month = 3
          else false
      | _ -> false)
  | "wedding_birthday" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> (
          match (get_relation fam, get_divorce fam) with
          | (Married | NoSexesCheckMarried), NotDivorced -> (
              match (m_auth, Date.cdate_to_dmy_opt (get_marriage fam)) with
              | true, Some d ->
                  let father = pget conf base (get_father fam) in
                  let mother = pget conf base (get_mother fam) in
                  if
                    d.prec = Sure
                    && authorized_age conf base father
                    && get_death father = NotDead
                    && authorized_age conf base mother
                    && get_death mother = NotDead
                  then
                    d.day = conf.today.day && d.month = conf.today.month
                    && d.year < conf.today.year
                    || (not (Date.leap_year conf.today.year))
                       && d.day = 29 && d.month = 2 && conf.today.day = 1
                       && conf.today.month = 3
                  else false
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | "computable_age" ->
      if p_auth then
        match (Date.cdate_to_dmy_opt (get_birth p), get_death p) with
        | Some d, NotDead -> not (d.day = 0 && d.month = 0 && d.prec <> Sure)
        | _ -> false
      else false
  | "computable_death_age" ->
      if p_auth then
        match Gutil.get_birth_death_date p with
        | ( Some (Dgreg (({ prec = Sure | About | Maybe } as d1), _)),
            Some (Dgreg (({ prec = Sure | About | Maybe } as d2), _)),
            _ )
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            a.year > 0
            || (a.year = 0 && (a.month > 0 || (a.month = 0 && a.day > 0)))
        | _ -> false
      else false
  | "computable_marriage_age" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          if m_auth then
            match
              ( Date.cdate_to_dmy_opt (get_birth p),
                Date.cdate_to_dmy_opt (get_marriage fam) )
            with
            | ( Some ({ prec = Sure | About | Maybe } as d1),
                Some ({ prec = Sure | About | Maybe } as d2) ) ->
                let a = Date.time_elapsed d1 d2 in
                a.year > 0
                || (a.year = 0 && (a.month > 0 || (a.month = 0 && a.day > 0)))
            | _ -> false
          else false
      | _ -> raise Not_found)
  | "has_approx_birth_date" ->
      p_auth && fst (Util.get_approx_birth_date_place conf base p) <> None
  | "has_approx_birth_place" ->
      p_auth
      && (snd (Util.get_approx_birth_date_place conf base p) :> string) <> ""
  | "has_approx_death_date" ->
      p_auth && fst (Util.get_approx_death_date_place conf base p) <> None
  | "has_approx_death_place" ->
      p_auth
      && (snd (Util.get_approx_death_date_place conf base p) :> string) <> ""
  | "has_aliases" ->
      if (not p_auth) && is_hide_names conf p then false
      else get_aliases p <> []
  | "has_baptism_date" -> p_auth && get_baptism p <> Date.cdate_None
  | "has_baptism_place" -> p_auth && sou base (get_baptism_place p) <> ""
  | "has_baptism_source" -> p_auth && sou base (get_baptism_src p) <> ""
  | "has_baptism_note" ->
      p_auth && (not conf.no_note) && sou base (get_baptism_note p) <> ""
  | "has_baptism_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Epers_Baptism)
  | "has_birth_date" -> p_auth && get_birth p <> Date.cdate_None
  | "has_birth_place" -> p_auth && sou base (get_birth_place p) <> ""
  | "has_birth_source" -> p_auth && sou base (get_birth_src p) <> ""
  | "has_birth_note" ->
      p_auth && (not conf.no_note) && sou base (get_birth_note p) <> ""
  | "has_birth_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Epers_Birth)
  | "has_burial_date" ->
      if p_auth then
        match get_burial p with
        | Buried cod -> Date.od_of_cdate cod <> None
        | Cremated _ | UnknownBurial -> false
      else false
  | "has_burial_place" -> p_auth && sou base (get_burial_place p) <> ""
  | "has_burial_source" -> p_auth && sou base (get_burial_src p) <> ""
  | "has_burial_note" ->
      p_auth && (not conf.no_note) && sou base (get_burial_note p) <> ""
  | "has_burial_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Epers_Burial)
  | "has_children" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) ->
          if Array.length (get_children fam) > 0 then true
          else !GWPARAM_ITL.has_children conf base p fam
      | _ -> (
          Array.exists
            (fun ifam -> [||] <> get_children (foi base ifam))
            (get_family p)
          ||
          match get_env "fam_link" env with
          | Vfam (ifam, _, (ifath, imoth, _), _) ->
              let conf =
                match get_env "baseprefix" env with
                | Vstring baseprefix -> { conf with command = baseprefix }
                | _ -> conf
              in
              []
              <> !GWPARAM_ITL.get_children_of_parents
                   base conf.command ifam ifath imoth
          | _ -> false))
  | "has_consanguinity" ->
      p_auth
      && get_consang p != Adef.fix (-1)
      && get_consang p >= Adef.fix_of_float 0.0001
  | "has_cremation_date" ->
      if p_auth then
        match get_burial p with
        | Cremated cod -> Date.od_of_cdate cod <> None
        | Buried _ | UnknownBurial -> false
      else false
  | "has_cremation_place" -> p_auth && sou base (get_burial_place p) <> ""
  | "has_cremation_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Epers_Cremation)
  | "has_death_date" -> (
      match get_death p with
      | Death (_, _) -> p_auth
      | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead
        ->
          false)
  | "has_death_place" -> p_auth && sou base (get_death_place p) <> ""
  | "has_death_source" -> p_auth && sou base (get_death_src p) <> ""
  | "has_death_note" ->
      p_auth && (not conf.no_note) && sou base (get_death_note p) <> ""
  | "has_death_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Epers_Death)
  | "has_event" ->
      if p_auth then
        let events = Event.events conf base p in
        let nb_fam = Array.length (get_family p) in
        match List.assoc_opt "has_events" conf.base_env with
        | Some "never" -> false
        | Some "always" ->
            if nb_fam > 0 || List.length events > 0 then true else false
        | Some _ | None ->
            (* Renvoie vrai que si il y a des informations supplémentaires *)
            (* par rapport aux évènements principaux, i.e. témoins (mais   *)
            (* on ne prend pas en compte les notes).                       *)
            let rec loop events nb_birth nb_bapt nb_deat nb_buri nb_marr =
              match events with
              | [] -> false
              | (name, _, p, n, s, wl, _) :: events -> (
                  let p, n, s = (sou base p, sou base n, sou base s) in
                  match name with
                  | Event.Pevent pname -> (
                      match pname with
                      | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
                      | Epers_Cremation ->
                          if Array.length wl > 0 then true
                          else
                            let nb_birth, nb_bapt, nb_deat, nb_buri =
                              match pname with
                              | Epers_Birth ->
                                  (succ nb_birth, nb_bapt, nb_deat, nb_buri)
                              | Epers_Baptism ->
                                  (nb_birth, succ nb_bapt, nb_deat, nb_buri)
                              | Epers_Death ->
                                  (nb_birth, nb_bapt, succ nb_deat, nb_buri)
                              | Epers_Burial | Epers_Cremation ->
                                  (nb_birth, nb_bapt, nb_deat, succ nb_buri)
                              | _ -> (nb_birth, nb_bapt, nb_deat, nb_buri)
                            in
                            if
                              Array.exists
                                (fun i -> i > 1)
                                [| nb_birth; nb_bapt; nb_deat; nb_buri |]
                            then true
                            else
                              loop events nb_birth nb_bapt nb_deat nb_buri
                                nb_marr
                      | _ -> true)
                  | Fevent fname -> (
                      match fname with
                      | Efam_Engage | Efam_Marriage | Efam_NoMention
                      | Efam_NoMarriage ->
                          let nb_marr = succ nb_marr in
                          if nb_marr > nb_fam then true
                          else
                            loop events nb_birth nb_bapt nb_deat nb_buri nb_marr
                      | Efam_Divorce | Efam_Separated ->
                          if
                            p <> "" || n <> "" || s <> "" || Array.length wl > 0
                          then true
                          else
                            loop events nb_birth nb_bapt nb_deat nb_buri nb_marr
                      | _ -> true))
            in
            loop events 0 0 0 0 0
      else false
  | "has_families" ->
      Array.length (get_family p) > 0
      || !GWPARAM_ITL.has_family_correspondance conf.command (get_iper p)
  | "has_first_names_aliases" ->
      if (not p_auth) && is_hide_names conf p then false
      else get_first_names_aliases p <> []
  | "has_history" -> has_history conf base p p_auth
  | "has_image" | "has_portrait" ->
      Image.get_portrait conf base p |> Option.is_some
  | "has_image_url" | "has_portrait_url" -> (
      match Image.get_portrait conf base p with
      | Some (`Url _url) -> true
      | _ -> false)
  | "has_old_image_url" | "has_old_portrait_url" -> (
      match Image.get_old_portrait conf base p with
      | Some (`Url _url) -> true
      | _ -> false)
  (* carrousel *)
  | "has_carrousel" -> Image.get_carrousel_imgs conf base p <> []
  | "has_old_carrousel" -> Image.get_carrousel_old_imgs conf base p <> []
  | "has_old_image" | "has_old_portrait" ->
      Image.get_old_portrait conf base p |> Option.is_some
  | "has_nephews_or_nieces" -> has_nephews_or_nieces conf base p
  | "has_nobility_titles" -> p_auth && Util.nobtit conf base p <> []
  | "has_notes" | "has_pnotes" ->
      p_auth && (not conf.no_note) && sou base (get_notes p) <> ""
  | "has_occupation" -> p_auth && sou base (get_occupation p) <> ""
  | "has_parents" ->
      get_parents p <> None
      ||
      let conf =
        match get_env "baseprefix" env with
        | Vstring baseprefix -> { conf with command = baseprefix }
        | _ -> conf
      in
      !GWPARAM_ITL.has_parents_link conf.command (get_iper p)
  | "has_possible_duplications" -> has_possible_duplications conf base p
  | "has_psources" ->
      if is_hide_names conf p && not p_auth then false
      else sou base (get_psources p) <> ""
  | "has_public_name" ->
      if (not p_auth) && is_hide_names conf p then false
      else sou base (get_public_name p) <> ""
  | "has_qualifiers" ->
      if (not p_auth) && is_hide_names conf p then false
      else get_qualifiers p <> []
  | "has_relations" ->
      if p_auth && conf.use_restrict then
        let related =
          List.fold_left
            (fun l ip ->
              let rp = pget conf base ip in
              if is_hidden rp then l else ip :: l)
            [] (get_related p)
        in
        get_rparents p <> [] || related <> []
      else p_auth && (get_rparents p <> [] || get_related p <> [])
  | "has_siblings" -> (
      match get_parents p with
      | Some ifam -> Array.length (get_children (foi base ifam)) > 1
      | None ->
          let conf =
            match get_env "baseprefix" env with
            | Vstring baseprefix -> { conf with command = baseprefix }
            | _ -> conf
          in
          !GWPARAM_ITL.has_siblings conf.command (get_iper p))
  | "has_sources" ->
      p_auth
      && (sou base (get_psources p) <> ""
         || sou base (get_birth_src p) <> ""
         || sou base (get_baptism_src p) <> ""
         || sou base (get_death_src p) <> ""
         || sou base (get_burial_src p) <> ""
         || Array.exists
              (fun ifam ->
                let fam = foi base ifam in
                let isp = Gutil.spouse (get_iper p) fam in
                let sp = poi base isp in
                (* On sait que p_auth vaut vrai. *)
                let m_auth = authorized_age conf base sp in
                m_auth
                && (sou base (get_marriage_src fam) <> ""
                   || sou base (get_fsources fam) <> ""))
              (get_family p))
  | "has_surnames_aliases" ->
      if (not p_auth) && is_hide_names conf p then false
      else get_surnames_aliases p <> []
  | "is_buried" -> (
      match get_burial p with
      | Buried _ -> p_auth
      | Cremated _ | UnknownBurial -> false)
  | "is_cremated" -> (
      match get_burial p with
      | Cremated _ -> p_auth
      | Buried _ | UnknownBurial -> false)
  | "is_dead" -> (
      match get_death p with
      | Death _ | DeadYoung | DeadDontKnowWhen -> p_auth
      | NotDead | DontKnowIfDead | OfCourseDead -> false)
  | "is_certainly_dead" -> (
      match get_death p with
      | OfCourseDead -> p_auth
      (* TODOWHY : why not: | Death _ | DeadYoung -> true *)
      | Death _ | DeadYoung | DeadDontKnowWhen | NotDead | DontKnowIfDead ->
          false)
  | "is_descendant" -> (
      match get_env "desc_mark" env with
      | Vdmark r -> Gwdb.Marker.get !r (get_iper p)
      | _ -> raise Not_found)
  | "is_female" -> get_sex p = Female
  | "is_invisible" ->
      let conf = { conf with wizard = false; friend = false } in
      not (authorized_age conf base p)
  | "is_male" -> get_sex p = Male
  | "is_private" -> get_access p = Private
  | "is_public" -> Util.is_public conf base p
  | "is_restricted" -> is_hidden p
  | _ -> raise Not_found

and eval_str_person_field conf base env ((p, p_auth) as ep) = function
  | "access" -> acces conf base p |> safe_val
  | "age" -> (
      match (p_auth, Date.cdate_to_dmy_opt (get_birth p), get_death p) with
      | true, Some d, NotDead ->
          Date.time_elapsed d conf.today
          |> DateDisplay.string_of_age conf
          |> safe_val
      | _ -> null_val)
  | "alias" -> (
      match get_aliases p with
      | nn :: _ ->
          if (not p_auth) && is_hide_names conf p then null_val
          else sou base nn |> Util.escape_html |> safe_val
      | _ -> null_val)
  | "approx_birth_place" ->
      if p_auth then
        Util.get_approx_birth_date_place conf base p |> snd |> safe_val
      else null_val
  | "approx_death_place" ->
      if p_auth then
        Util.get_approx_death_date_place conf base p |> snd |> safe_val
      else null_val
  | "auto_image_file_name" -> (
      (* TODO what do we want here? can we remove this? *)
      match Image.get_portrait_path conf base p with
      | Some (`Path s) -> str_val s
      | None -> null_val)
  | "birth_place" ->
      if p_auth then
        get_birth_place p |> sou base |> Util.string_of_place conf |> safe_val
      else null_val
  | "birth_place_raw" ->
      if p_auth then sou base (get_birth_place p) |> str_val else null_val
  | "birth_note" ->
      get_birth_note p |> get_note_source conf base ~p p_auth conf.no_note
  | "birth_source" ->
      get_birth_src p |> get_note_source conf base ~p p_auth false
  | "baptism_place" ->
      if p_auth then
        get_baptism_place p |> sou base |> Util.string_of_place conf |> safe_val
      else null_val
  | "baptism_place_raw" ->
      if p_auth then sou base (get_baptism_place p) |> str_val else null_val
  | "baptism_note" ->
      get_baptism_note p |> get_note_source conf base ~p p_auth conf.no_note
  | "baptism_source" ->
      get_baptism_src p |> get_note_source conf base ~p p_auth false
  | "burial_place" ->
      if p_auth then
        get_burial_place p |> sou base |> Util.string_of_place conf |> safe_val
      else null_val
  | "burial_place_raw" ->
      if p_auth then sou base (get_burial_place p) |> str_val else null_val
  | "burial_note" ->
      get_burial_note p |> get_note_source conf base ~p p_auth conf.no_note
  | "burial_source" ->
      get_burial_src p |> get_note_source conf base ~p p_auth false
  | "child_name" ->
      let force_surname =
        match get_parents p with
        | None -> false
        | Some ifam ->
            foi base ifam |> get_father |> pget conf base |> p_surname base
            |> ( <> ) (p_surname base p)
      in
      if (not p_auth) && is_hide_names conf p then str_val "x x"
      else if force_surname then gen_person_text conf base p |> safe_val
      else gen_person_text ~sn:false ~chk:false conf base p |> safe_val
  | "consanguinity" ->
      if p_auth then
        string_of_decimal_num conf
          (round_2_dec (Adef.float_of_fix (get_consang p) *. 100.0))
        ^ " %"
        |> str_val
      else null_val
  | "cremation_place" ->
      if p_auth then
        get_burial_place p |> sou base |> Util.string_of_place conf |> safe_val
      else null_val
  | "cremation_place_raw" ->
      if p_auth then sou base (get_burial_place p) |> str_val else null_val
  | "dates" ->
      if p_auth then DateDisplay.short_dates_text conf base p |> safe_val
      else null_val
  | "dates_notag" ->
      if p_auth then DateDisplay.short_dates_text_notag conf base p |> str_val
      else null_val
  | "death_age" ->
      if p_auth then
        match Gutil.get_birth_death_date p with
        | ( Some (Dgreg (({ prec = Sure | About | Maybe } as d1), _)),
            Some (Dgreg (({ prec = Sure | About | Maybe } as d2), _)),
            approx )
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            let s =
              if (not approx) && d1.prec = Sure && d2.prec = Sure then ""
              else transl_decline conf "possibly (date)" "" ^ " "
            in
            s ^<^ DateDisplay.string_of_age conf a |> safe_val
        | _ -> null_val
      else null_val
  | "death_place" ->
      if p_auth then
        get_death_place p |> sou base |> Util.string_of_place conf |> safe_val
      else null_val
  | "death_place_raw" ->
      if p_auth then sou base (get_death_place p) |> str_val else null_val
  | "death_note" ->
      get_death_note p |> get_note_source conf base ~p p_auth conf.no_note
  | "death_source" ->
      get_death_src p |> get_note_source conf base ~p p_auth false
  | "died" -> string_of_died conf p p_auth |> safe_val
  | "father_age_at_birth" ->
      string_of_parent_age conf base ep get_father |> safe_val
  | "first_name" ->
      if (not p_auth) && is_hide_names conf p then str_val "x"
      else p_first_name base p |> Util.escape_html |> safe_val
  | "first_name_key" ->
      if is_hide_names conf p && not p_auth then null_val
      else p_first_name base p |> Name.lower |> Mutil.encode |> safe_val
  | "first_name_key_val" ->
      if is_hide_names conf p && not p_auth then null_val
      else p_first_name base p |> Name.lower |> str_val
  | "first_name_key_strip" ->
      if is_hide_names conf p && not p_auth then null_val
      else Name.strip_c (p_first_name base p) '"' |> str_val
  | "history_file" ->
      if not p_auth then null_val
      else
        let fn = sou base (get_first_name p) in
        let sn = sou base (get_surname p) in
        let occ = get_occ p in
        HistoryDiff.history_file fn sn occ |> str_val
  | "image" -> (
      match Image.get_portrait conf base p with
      | Some src -> Image.src_to_string src |> str_val
      | None -> null_val)
  | "image_html_url" -> string_of_image_url conf base ep true |> safe_val
  | "image_size" -> string_of_image_size conf base ep |> str_val
  | "image_medium_size" -> string_of_image_medium_size conf base ep |> str_val
  | "image_small_size" -> string_of_image_small_size conf base ep |> str_val
  | "image_url" -> string_of_image_url conf base ep false |> safe_val
  | "index" -> (
      match get_env "p_link" env with
      | Vbool _ -> null_val
      | _ -> get_iper p |> string_of_iper |> Mutil.encode |> safe_val)
  (* carrousel functions *)
  | "carrousel" -> Image.default_portrait_filename base p |> str_val
  | "carrousel_img_nbr" ->
      string_of_int (List.length (Image.get_carrousel_imgs conf base p))
      |> str_val
  | "carrousel_old_img_nbr" ->
      string_of_int (List.length (Image.get_carrousel_old_imgs conf base p))
      |> str_val
  | "carrousel_img_note" -> (
      match get_env "carrousel_img_note" env with
      | Vstring note -> str_val note
      | _ -> raise Not_found)
  | "carrousel_img_src" -> (
      match get_env "carrousel_img_src" env with
      | Vstring source -> str_val source
      | _ -> raise Not_found)
  | "portrait" -> (
      (* TODO what do we want here? can we remove this? *)
      match Image.get_portrait conf base p with
      | Some (`Path s) -> str_val s
      | Some (`Url u) -> str_val u
      | None -> null_val)
  | "portrait_name" -> (
      match Image.get_portrait conf base p with
      | Some (`Path s) -> str_val (Filename.basename s)
      | Some (`Url u) -> str_val u (* ?? *)
      | None -> null_val)
  | "portrait_saved" -> (
      match Image.get_old_portrait conf base p with
      | Some (`Path s) -> str_val s
      | Some (`Url u) -> str_val u
      | None -> null_val)
  | "portrait_saved_name" -> (
      match Image.get_old_portrait conf base p with
      | Some (`Path s) -> str_val (Filename.basename s)
      | Some (`Url u) -> str_val u (* ?? *)
      | None -> null_val)
  | "X" -> str_val Filename.dir_sep (* end carrousel functions *)
  | "key" ->
      if is_hide_names conf p && not p_auth then null_val
      else
        Format.sprintf "%s.%d %s"
          (p_first_name base p |> Name.lower)
          (get_occ p)
          (p_surname base p |> Name.lower)
        |> str_val
  | "mark_descendants" -> (
      match get_env "desc_mark" env with
      | Vdmark r ->
          let tab = Gwdb.iper_marker (Gwdb.ipers base) false in
          let rec mark_descendants len p =
            let i = get_iper p in
            if Gwdb.Marker.get tab i then ()
            else (
              Gwdb.Marker.set tab i true;
              let u = p in
              for i = 0 to Array.length (get_family u) - 1 do
                let des = foi base (get_family u).(i) in
                for i = 0 to Array.length (get_children des) - 1 do
                  mark_descendants (len + 1)
                    (pget conf base (get_children des).(i))
                done
              done)
          in
          mark_descendants 0 p;
          r := tab;
          null_val
      | _ -> raise Not_found)
  | "marriage_age" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          if m_auth then
            match
              ( Date.cdate_to_dmy_opt (get_birth p),
                Date.cdate_to_dmy_opt (get_marriage fam) )
            with
            | ( Some ({ prec = Sure | About | Maybe } as d1),
                Some ({ prec = Sure | About | Maybe } as d2) ) ->
                Date.time_elapsed d1 d2
                |> DateDisplay.string_of_age conf
                |> safe_val
            | _ -> null_val
          else null_val
      | _ -> raise Not_found)
  | "marriage_places" ->
      List.fold_left
        (fun acc ifam ->
          acc
          ^ (if acc = "" then "" else "|")
          ^ sou base (get_marriage_place (foi base ifam)))
        ""
        (Array.to_list (get_family p))
      |> str_val
  | "mother_age_at_birth" ->
      string_of_parent_age conf base ep get_mother |> safe_val
  | "misc_names" ->
      if p_auth then
        let l =
          Util.nobtit conf base
          |> Gwdb.person_misc_names base p
          |> List.map Util.escape_html
        in
        let l =
          let first_name = p_first_name base p in
          let surname = p_surname base p in
          if first_name <> "?" && surname <> "?" then
            (first_name ^ " " ^ surname |> Name.lower |> Util.escape_html) :: l
          else l
        in
        if l <> [] then
          "<ul>"
          ^<^ List.fold_left
                (fun s n -> s ^^^ "<li>" ^<^ n ^>^ "</li>")
                (Adef.safe "")
                (l : Adef.escaped_string list :> Adef.safe_string list)
          ^>^ "</ul>"
          |> safe_val
        else null_val
      else null_val
  | "nb_children_total" ->
      Array.fold_left
        (fun n ifam -> n + Array.length (get_children (foi base ifam)))
        0 (get_family p)
      |> string_of_int |> str_val
  | "nb_children" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) ->
          get_children fam |> Array.length |> string_of_int |> str_val
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (ifam, _, _, _) ->
              let baseprefix =
                match get_env "baseprefix" env with
                | Vstring baseprefix -> baseprefix
                | _ -> conf.command
              in
              string_of_int (!GWPARAM_ITL.nb_children baseprefix ifam)
              |> str_val
          | _ ->
              Array.fold_left
                (fun n ifam -> n + Array.length (get_children (foi base ifam)))
                0 (get_family p)
              |> string_of_int |> str_val))
  | "nb_families" -> (
      match get_env "p_link" env with
      | Vbool _ ->
          get_iper p
          |> !GWPARAM_ITL.nb_families conf.command
          |> string_of_int |> str_val
      | _ -> get_family p |> Array.length |> string_of_int |> str_val)
  | "notes" | "pnotes" ->
      get_notes p |> get_note_source conf base ~p p_auth conf.no_note
  | "occ" ->
      if is_hide_names conf p && not p_auth then null_val
      else get_occ p |> string_of_int |> str_val
  | "occupation" ->
      get_occupation p |> get_note_source conf base ~p p_auth false
  | "on_baptism_date" -> date_aux conf p_auth (get_baptism p)
  | "slash_baptism_date" ->
      if p_auth then
        match Date.od_of_cdate (get_baptism p) with
        | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
        | None -> null_val
      else null_val
  | "on_birth_date" -> date_aux conf p_auth (get_birth p)
  | "slash_birth_date" ->
      if p_auth then
        match Date.od_of_cdate (get_birth p) with
        | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
        | None -> null_val
      else null_val
  | "slash_approx_birth_date" ->
      if p_auth then
        match fst (Util.get_approx_birth_date_place conf base p) with
        | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
        | None -> null_val
      else null_val
  | "on_burial_date" -> (
      match get_burial p with
      | Buried cod -> date_aux conf p_auth cod
      | Cremated _ | UnknownBurial -> raise Not_found)
  | "psources" -> get_psources p |> get_note_source conf base ~p p_auth false
  | "slash_burial_date" ->
      if p_auth then
        match get_burial p with
        | Buried cod -> (
            match Date.od_of_cdate cod with
            | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
            | None -> null_val)
        | Cremated _ | UnknownBurial -> raise Not_found
      else null_val
  | "on_cremation_date" -> (
      match get_burial p with
      | Cremated cod -> date_aux conf p_auth cod
      | Buried _ | UnknownBurial -> raise Not_found)
  | "slash_cremation_date" -> (
      match get_burial p with
      | Cremated cod -> (
          match (p_auth, Date.od_of_cdate cod) with
          | true, Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
          | _ -> null_val)
      | _ -> raise Not_found)
  | "on_death_date" -> (
      match get_death p with
      | Death (_, d) -> date_aux conf p_auth d
      | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead
        ->
          raise Not_found)
  | "slash_death_date" -> (
      match (p_auth, get_death p) with
      | true, Death (_, d) ->
          Date.date_of_cdate d
          |> DateDisplay.string_slash_of_date conf
          |> safe_val
      | _ -> null_val)
  | "slash_approx_death_date" -> (
      match (p_auth, fst (Util.get_approx_death_date_place conf base p)) with
      | true, Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
      | _ -> null_val)
  | "prev_fam_father" -> (
      match get_env "prev_fam" env with
      | Vfam (_, _, (ifath, _, _), _) ->
          string_of_iper ifath |> Mutil.encode |> safe_val
      | _ -> raise Not_found)
  | "prev_fam_index" -> (
      match get_env "prev_fam" env with
      | Vfam (ifam, _, _, _) -> string_of_ifam ifam |> Mutil.encode |> safe_val
      | _ -> raise Not_found)
  | "prev_fam_mother" -> (
      match get_env "prev_fam" env with
      | Vfam (_, _, (_, imoth, _), _) ->
          string_of_iper imoth |> Mutil.encode |> safe_val
      | _ -> raise Not_found)
  | "public_name" ->
      if (not p_auth) && is_hide_names conf p then null_val
      else get_public_name p |> sou base |> Util.escape_html |> safe_val
  | "qualifier" -> (
      match get_qualifiers p with
      | nn :: _ when p_auth || not (is_hide_names conf p) ->
          sou base nn |> Util.escape_html |> safe_val
      | _ -> null_val)
  | "sex" ->
      (* Pour éviter les traductions bizarre, on ne teste pas p_auth. *)
      get_sex p |> index_of_sex |> string_of_int |> str_val
  | "sosa_in_list" -> (
      match get_env "all_gp" env with
      | Vallgp all_gp -> (
          match get_link all_gp (get_iper p) with
          | Some (GP_person (s, _, _)) -> str_val (Sosa.to_string s)
          | Some _ | None -> null_val)
      | _ -> raise Not_found)
  | "sosa_link" -> (
      match get_env "sosa" env with
      | Vsosa x -> (
          match get_sosa conf base env x p with
          | Some (n, q) ->
              Printf.sprintf "m=RL&i1=%s&i2=%s&b1=1&b2=%s"
                (string_of_iper (get_iper p))
                (string_of_iper (get_iper q))
                (Sosa.to_string n)
              |> str_val
          | None -> null_val)
      | _ -> raise Not_found)
  | "source" -> (
      match get_env "src" env with
      | Vstring s -> safe_val (Notes.source_note conf base p s)
      | _ -> raise Not_found)
  | "surname" ->
      if (not p_auth) && is_hide_names conf p then str_val "x"
      else p_surname base p |> Util.escape_html |> safe_val
  | "surname_begin" ->
      if (not p_auth) && is_hide_names conf p then null_val
      else
        p_surname base p |> surname_particle base |> Util.escape_html
        |> safe_val
  | "surname_end" ->
      if (not p_auth) && is_hide_names conf p then str_val "x"
      else
        p_surname base p
        |> surname_without_particle base
        |> Util.escape_html |> safe_val
  | "surname_key" ->
      if is_hide_names conf p && not p_auth then null_val
      else p_surname base p |> Name.lower |> Mutil.encode |> safe_val
  | "surname_key_val" ->
      if is_hide_names conf p && not p_auth then null_val
      else p_surname base p |> Name.lower |> str_val
  | "surname_key_strip" ->
      if is_hide_names conf p && not p_auth then null_val
      else Name.strip_c (p_surname base p) '"' |> str_val
  | "title" -> person_title conf base p |> safe_val
  | _ -> raise Not_found

and eval_witness_relation_var conf base env
    ((_, _, (ip1, ip2, _), m_auth) as fcd) loc = function
  | [] ->
      if not m_auth then null_val
      else
        Printf.sprintf
          (ftransl conf "witness at marriage of %s and %s")
          (pget conf base ip1 |> referenced_person_title_text conf base
            :> string)
          (pget conf base ip2 |> referenced_person_title_text conf base
            :> string)
        |> str_val
  | sl -> eval_family_field_var conf base env fcd loc sl

and eval_family_field_var conf base env
    ((_, fam, (ifath, imoth, _), m_auth) as fcd) loc = function
  | [ "date_s" ] | [ "dates" ] ->
      VVstring
        (DateDisplay.short_family_dates_text conf base true fam :> string)
  | "father" :: sl -> (
      match get_env "f_link" env with
      | Vbool _ -> raise Not_found
      | _ ->
          let ep = make_ep conf base ifath in
          eval_person_field_var conf base env ep loc sl)
  | "marriage_date" :: sl -> (
      match Date.od_of_cdate (get_marriage fam) with
      | Some d when m_auth -> eval_date_field_var conf d sl
      | Some _ | None -> null_val)
  | "mother" :: sl -> (
      match get_env "f_link" env with
      | Vbool _ -> raise Not_found
      | _ ->
          let ep = make_ep conf base imoth in
          eval_person_field_var conf base env ep loc sl)
  | "marriage" :: sl -> eval_family_marriage_field_var fam sl
  | [ "sep_date_s" ] | [ "sep_dates" ] ->
      VVstring
        (DateDisplay.short_family_dates_text conf base false fam :> string)
  | [ s ] -> str_val (eval_str_family_field env fcd s)
  | _ -> raise Not_found

and eval_family_marriage_field_var fam = function
  | [ "nb_witnesses_witness" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness |> string_of_int)
  | [ "nb_witnesses_godparent" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_GodParent
        |> string_of_int)
  | [ "nb_witnesses_civilofficer" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_CivilOfficer
        |> string_of_int)
  | [ "nb_witnesses_religiousofficer" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_ReligiousOfficer
        |> string_of_int)
  | [ "nb_witnesses_informant" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Informant
        |> string_of_int)
  | [ "nb_witnesses_attending" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Attending
        |> string_of_int)
  | [ "nb_witnesses_mentioned" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Mentioned
        |> string_of_int)
  | [ "nb_witnesses_other" ] ->
      VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Other
        |> string_of_int)
  | _ -> raise Not_found

and eval_str_family_field env (ifam, _, _, _) = function
  | "desc_level" -> (
      match get_env "desc_level_table" env with
      | Vdesclevtab levt ->
          let _, flevt = Lazy.force levt in
          string_of_int (Gwdb.Marker.get flevt ifam)
      | _ -> raise Not_found)
  | "index" -> string_of_ifam ifam
  | "set_infinite_desc_level" -> (
      match get_env "desc_level_table" env with
      | Vdesclevtab levt ->
          let _, flevt = Lazy.force levt in
          Gwdb.Marker.set flevt ifam infinite;
          ""
      | _ -> raise Not_found)
  | _ -> raise Not_found

and simple_person_text conf base p p_auth : Adef.safe_string =
  if p_auth then
    match main_title conf base p with
    | Some t -> titled_person_text conf base p t
    | None -> gen_person_text conf base p
  else if is_hide_names conf p then Adef.safe "x x"
  else gen_person_text conf base p

and string_of_died conf p p_auth =
  Adef.safe
  @@
  if p_auth then
    let is = index_of_sex (get_sex p) in
    match get_death p with
    | Death (dr, _) -> (
        match dr with
        | Unspecified -> transl_nth conf "died" is
        | Murdered -> transl_nth conf "murdered" is
        | Killed -> transl_nth conf "killed (in action)" is
        | Executed -> transl_nth conf "executed (legally killed)" is
        | Disappeared -> transl_nth conf "disappeared" is)
    | DeadYoung -> transl_nth conf "died young" is
    | DeadDontKnowWhen -> transl_nth conf "died" is
    | NotDead | DontKnowIfDead | OfCourseDead -> ""
  else ""

and string_of_image_url conf base (p, p_auth) html : Adef.escaped_string =
  if p_auth then
    match Image.get_portrait conf base p with
    | Some (`Path fname) ->
        let s = Unix.stat fname in
        let b = acces conf base p in
        let k = Image.default_portrait_filename base p in
        Format.sprintf "%sm=IM%s&d=%d&%s&k=/%s"
          (commd conf :> string)
          (if html then "H" else "")
          (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
          (b :> string)
          k
        |> Adef.escaped
    | Some (`Url url) -> Adef.escaped url (* FIXME *)
    | None -> Adef.escaped ""
  else Adef.escaped ""

and string_of_parent_age conf base (p, p_auth) parent : Adef.safe_string =
  match get_parents p with
  | Some ifam ->
      let cpl = foi base ifam in
      let pp = pget conf base (parent cpl) in
      if p_auth && authorized_age conf base pp then
        match
          ( Date.cdate_to_dmy_opt (get_birth pp),
            Date.cdate_to_dmy_opt (get_birth p) )
        with
        | Some d1, Some d2 ->
            Date.time_elapsed d1 d2 |> DateDisplay.string_of_age conf
        | _ -> Adef.safe ""
      else Adef.safe ""
  | None -> raise Not_found

and string_of_int_env var env =
  match get_env var env with
  | Vint x -> string_of_int x |> str_val
  | _ -> raise Not_found

let eval_transl conf base env upp s c =
  match c with
  | "n" | "s" | "w" | "f" | "c" | "e" | "t" ->
      let n =
        match c with
        | "n" -> (
            (* select nth value *)
            (* replaced by %apply;nth([...],sex) or "s" below *)
            match get_env "count" env with Vcnt i -> !i | _ -> 0)
        | "s" -> (
            (* male/female/neuter *)
            match get_env "child" env with
            | Vind p -> index_of_sex (get_sex p)
            | _ -> (
                match get_env "p" env with
                | Vind p -> index_of_sex (get_sex p)
                | _ ->
                    Printf.sprintf "Sex of unknown person"
                    |> !GWPARAM.syslog `LOG_WARNING;
                    assert false))
        | "w" -> (
            (* witness/witnesses *)
            match get_env "fam" env with
            | Vfam (_, fam, _, _) ->
                if Array.length (get_witnesses fam) <= 1 then 0 else 1
            | _ -> 0)
        | "f" -> (
            (* family/families *)
            match get_env "p" env with
            | Vind p -> if Array.length (get_family p) <= 1 then 0 else 1
            | _ ->
                Printf.sprintf "families of unknown person"
                |> !GWPARAM.syslog `LOG_WARNING;
                assert false)
        | "c" -> (
            (* child/children *)
            match get_env "fam" env with
            | Vfam (_, fam, _, _) ->
                if Array.length (get_children fam) <= 1 then 0 else 1
            | _ -> (
                match get_env "p" env with
                | Vind p ->
                    let n =
                      Array.fold_left
                        (fun n ifam ->
                          n + Array.length (get_children (foi base ifam)))
                        0 (get_family p)
                    in
                    if n <= 1 then 0 else 1
                | _ ->
                    Printf.sprintf "Children of unknown person"
                    |> !GWPARAM.syslog `LOG_WARNING;
                    assert false))
        | "e" -> (
            (* singular/plural for events *)
            match get_env "p" env with
            | Vind p -> (
                match Event.events conf base p with
                | [] -> 0
                | [ _e ] -> 0
                | _ -> 1)
            | _ ->
                Printf.sprintf "Events of unknown person"
                |> !GWPARAM.syslog `LOG_WARNING;
                assert false)
        | "t" -> (
            (* singular/plural  titles *)
            match get_env "p" env with
            | Vind p -> (
                match Util.nobtit conf base p with
                | [] -> 0
                | [ _t ] -> 0
                | _ -> 1)
            | _ ->
                Printf.sprintf "Titles of unknown person"
                |> !GWPARAM.syslog `LOG_WARNING;
                assert false)
        | _ -> assert false
      in
      let r = Templ.eval_transl_lexicon conf upp s (string_of_int n) in
      if upp then Utf8.capitalize_fst r else r
  | _ -> Templ.eval_transl conf upp s c

let level_in_list in_or_less level lev_list =
  match lev_list with
  | [] -> None
  | lev_list ->
      List.find_opt
        (fun lvl -> if in_or_less then level = abs lvl else level <= abs lvl)
        lev_list

let print_foreach conf base print_ast eval_expr =
  let eval_int_expr env ep e =
    let s = eval_expr env ep e in
    try int_of_string s with Failure _ -> raise Not_found
  in

  let print_foreach_alias env al ((p, p_auth) as ep) =
    if (not p_auth) && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first a ->
          let env = ("alias", Vstring (sou base a)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (get_aliases p)
  in

  let print_foreach_ascendant env al ep =
    match get_env "gpl" env with
    | Vgpl gpl ->
        let rec loop first gpl =
          match gpl with
          | [] -> ()
          | gp :: gl ->
              (match gp with
              | GP_missing (_, _) -> ()
              | GP_person _ | GP_same _ | GP_interv _ ->
                  let env =
                    ("ancestor", Vanc gp) :: ("first", Vbool first)
                    :: ("last", Vbool (gl = []))
                    :: ("mode", Vstring "Vgpl") :: env
                  in
                  List.iter (print_ast env ep) al);
              loop false gl
        in
        loop true gpl
    | _ -> ()
  in

  let print_foreach_path_aux conf base in_or_less level env al ep l =
    let rec loop first cnt l =
      match l with
      | [] -> ()
      | (ip, (_, iancl, nbr), lev_list) :: l -> (
          match level_in_list in_or_less level lev_list with
          | Some lev ->
              (let lev_cnt = List.length lev_list in
               let ianc_env =
                 match iancl with
                 | ianc1 :: ianc2 :: _ ->
                     [
                       ("anc1", Vind (pget conf base ianc1));
                       ("anc2", Vind (pget conf base ianc2));
                     ]
                 | ianc1 :: _ ->
                     [
                       ("anc1", Vind (pget conf base ianc1));
                       ("anc2", Vind (poi base Gwdb.dummy_iper));
                     ]
                 | _ ->
                     [
                       ("anc1", Vind (poi base Gwdb.dummy_iper));
                       ("anc2", Vind (poi base Gwdb.dummy_iper));
                     ]
               in
               let env =
                 ("path_end", Vind (poi base ip))
                 :: ("anc_level", Vint lev) :: ("lev_cnt", Vint lev_cnt)
                 :: ("first", Vbool first) :: ("cnt", Vint cnt)
                 :: ("nbr", Vint nbr)
                 :: ("last", Vbool (l = []))
                 :: env
                 @ ianc_env
               in
               List.iter (print_ast env ep) al);
              loop false (cnt + 1) l
          | None -> loop false (cnt + 1) l)
    in
    loop true 1 (List.rev l)
  in

  let print_foreach_ascendant_level env el al ((p, _) as ep) =
    let max_level =
      match el with
      | [ [ e ] ] -> eval_int_expr env ep e
      | [] -> ( match get_env "max_anc_level" env with Vint n -> n | _ -> 0)
      | _ -> raise Not_found
    in
    let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
    let rec loop gpl i n =
      let prev_n = n in
      if i > max_level then ()
      else
        let n =
          List.fold_left
            (fun n gp ->
              match gp with
              | GP_person (_, _, _) -> n + 1
              | GP_same _ | GP_interv _ | GP_missing _ -> n)
            n gpl
        in
        let env =
          ("gpl", Vgpl gpl) :: ("level", Vint i)
          :: ("nbr_a", Vint (n - 1))
          :: ("nbr_a_l", Vint (n - prev_n))
          :: env
        in
        List.iter (print_ast env ep) al;
        let gpl = next_generation conf base mark gpl in
        loop gpl (succ i) n
    in
    loop [ GP_person (Sosa.one, get_iper p, None) ] 0 0
  in

  let print_foreach_ascendant_at_level env al ((p, _) as ep) =
    let max_lev = "max_anc_level" in
    let max_level = match get_env max_lev env with Vint n -> n | _ -> 0 in
    let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
    let rec loop gpl i =
      if i > max_level then ()
      else
        let env = ("gpl", Vgpl gpl) :: ("level", Vint i) :: env in
        List.iter (print_ast env ep) al;
        Gwdb.Collection.iter
          (fun i -> Gwdb.Marker.set mark i Sosa.zero)
          (Gwdb.ipers base);
        let gpl = next_generation2 conf base mark gpl in
        loop gpl (succ i)
    in
    loop [ GP_person (Sosa.one, get_iper p, None) ] 0
  in

  let print_foreach_anc_surn env el al loc ((p, _) as ep) =
    let max_level =
      match el with
      | [ [ e ] ] -> eval_int_expr env ep e
      | [] -> ( match get_env "max_anc_level" env with Vint n -> n | _ -> 0)
      | _ -> raise Not_found
    in
    (* En fonction du type de sortie demandé, on construit *)
    (* soit la liste des branches soit la liste éclair.    *)
    match p_getenv conf.env "t" with
    | Some "E" ->
        let l = build_list_eclair conf base max_level p in
        List.iter
          (fun (a, b, c, d, e, f) ->
            let b = (b : Adef.escaped_string :> Adef.safe_string) in
            let env =
              ("ancestor", Vanc_surn (Eclair (a, b, c, d, e, f, loc))) :: env
            in
            List.iter (print_ast env ep) al)
          l
    | Some "F" ->
        let l = build_surnames_list conf base max_level p in
        List.iter
          (fun (a, (((b, c, d), e), f)) ->
            let env =
              ("ancestor", Vanc_surn (Branch (a, b, c, d, e, f, loc))) :: env
            in
            List.iter (print_ast env ep) al)
          l
    | _ -> ()
  in

  let print_foreach_ancestor_tree env el al ((p, _) as ep) =
    let p, max_level =
      match el with
      | [ [ e1 ]; [ e2 ] ] ->
          let ip = iper_of_string @@ eval_expr env ep e1 in
          let max_level = eval_int_expr env ep e2 in
          (pget conf base ip, max_level)
      | [ [ e ] ] -> (p, eval_int_expr env ep e)
      | [] -> (
          match get_env "max_anc_level" env with Vint n -> (p, n) | _ -> (p, 0))
      | _ -> raise Not_found
    in
    let gen = tree_generation_list conf base max_level p in
    let rec loop first = function
      | g :: gl ->
          let env =
            ("celll", Vcelll g) :: ("first", Vbool first)
            :: ("last", Vbool (gl = []))
            :: env
          in
          List.iter (print_ast env ep) al;
          loop false gl
      | [] -> ()
    in
    loop true gen
  in
  let print_foreach_cell env al ep =
    let celll =
      match get_env "celll" env with
      | Vcelll celll -> celll
      | _ -> raise Not_found
    in
    Mutil.list_iter_first
      (fun first cell ->
        let env = ("cell", Vcell cell) :: ("first", Vbool first) :: env in
        List.iter (print_ast env ep) al)
      celll
  in
  let print_foreach_child env al ep = function
    | Vfam (ifam, fam, (ifath, imoth, isp), _) -> (
        match get_env "f_link" env with
        | Vbool _ ->
            let baseprefix =
              match get_env "baseprefix" env with
              | Vstring baseprefix -> baseprefix
              | _ -> conf.command
            in
            let children =
              !GWPARAM_ITL.get_children base baseprefix ifam ifath imoth
            in
            List.iter
              (fun (((p, _) as ep), baseprefix) ->
                let env = ("#loop", Vint 0) :: env in
                let env = ("child_link", Vind p) :: env in
                let env = ("baseprefix", Vstring baseprefix) :: env in
                let env = ("p_link", Vbool true) :: env in
                List.iter (print_ast env ep) al)
              children
        | _ ->
            let auth =
              Array.for_all
                (fun ip -> authorized_age conf base (pget conf base ip))
                (get_children fam)
            in
            let env = ("auth", Vbool auth) :: env in
            let n =
              let p =
                match get_env "p" env with Vind p -> p | _ -> assert false
              in
              let rec loop i =
                if i = Array.length (get_children fam) then -2
                else if (get_children fam).(i) = get_iper p then i
                else loop (i + 1)
              in
              loop 0
            in
            Array.iteri
              (fun i ip ->
                let p = pget conf base ip in
                let env =
                  ("#loop", Vint 0) :: ("child", Vind p)
                  :: ("child_cnt", Vint (i + 1))
                  :: env
                in
                let env =
                  if i = n - 1 && not (is_hidden p) then
                    ("pos", Vstring "prev") :: env
                  else if i = n then ("pos", Vstring "self") :: env
                  else if i = n + 1 && not (is_hidden p) then
                    ("pos", Vstring "next") :: env
                  else env
                in
                let ep = (p, authorized_age conf base p) in
                List.iter (print_ast env ep) al)
              (get_children fam);

            List.iter
              (fun (_, _, children) ->
                List.iter
                  (fun ((p, _), baseprefix, can_merge) ->
                    if not can_merge then
                      let env =
                        ("#loop", Vint 0) :: ("child_link", Vind p)
                        :: ("baseprefix", Vstring baseprefix)
                        :: ("p_link", Vbool true) :: env
                      in
                      let ep = (p, true) in
                      List.iter (print_ast env ep) al)
                  children)
              (!GWPARAM_ITL.get_children' conf base (get_iper (fst ep)) fam isp)
        )
    | _ -> ()
  in
  let print_foreach_descendant env al (p, _) count_paths =
    let lev =
      match get_env "level" env with
      | Vint lev -> lev
      | _ ->
          !GWPARAM.syslog `LOG_WARNING "Missing level info";
          0
    in
    let ip_l =
      match get_env "cousins" env with
      | Vcousl cl -> !cl
      | _ ->
          !GWPARAM.syslog `LOG_WARNING "Empty cousins list";
          []
    in
    let ip_l =
      if count_paths then List.map (fun (ip, (_, _, _), _) -> ip) ip_l
      else get_descendants_at_level base p lev
    in
    let rec loop i ip_l =
      match ip_l with
      | [] -> ()
      | ip :: ip_l ->
          let ep = (poi base ip, true) in
          let env =
            ("descendant", Vind (poi base ip))
            :: ("nbr", Vint i)
            :: ("first", Vbool (i = 0))
            :: ("last", Vbool (ip_l = []))
            :: env
          in
          List.iter (print_ast env ep) al;
          loop (succ i) ip_l
    in
    loop 0 ip_l
  in
  let print_foreach_descendant_level env al ep =
    let max_level =
      match get_env "max_desc_level" env with Vint n -> n | _ -> 0
    in
    let rec loop i =
      if i > max_level then ()
      else
        let env = ("level", Vint i) :: env in
        List.iter (print_ast env ep) al;
        loop (succ i)
    in
    loop 0
  in
  let print_foreach_event env al ((p, _) as ep) =
    Mutil.list_iter_first
      (fun first evt ->
        let env = ("event", Vevent (p, evt)) :: env in
        let env = ("first", Vbool first) :: env in
        List.iter (print_ast env ep) al)
      (Event.sorted_events conf base p)
  in
  let print_foreach_epers_event_witness env al ((p, _) as ep) epers_event =
    let epers_event_witness_string =
      match epers_event with
      | Epers_Burial -> "burial_witness"
      | Epers_Cremation -> "cremation_witness"
      | Epers_Death -> "death_witness"
      | Epers_Baptism -> "batism_witness"
      | Epers_Birth -> "birth_witness"
      | _ -> "witness"
    in
    List.iter
      (fun (name, _, _, _, _, wl, _) ->
        if name = Event.Pevent epers_event then
          Array.iteri
            (fun i (ip, _) ->
              let p = pget conf base ip in
              let env =
                (epers_event_witness_string, Vind p)
                :: ("first", Vbool (i = 0))
                :: env
              in
              List.iter (print_ast env ep) al)
            wl
        else ())
      (Event.sorted_events conf base p)
  in
  let print_foreach_event_witness env al ((_, p_auth) as ep) =
    if p_auth then
      match get_env "event" env with
      | Vevent (_, (_, _, _, _, _, witnesses, _)) ->
          Array.iteri
            (fun i (ip, wk) ->
              let p = pget conf base ip in
              let wk = Util.string_of_witness_kind conf (get_sex p) wk in
              let env =
                ("event_witness", Vind p)
                :: ("event_witness_kind", Vstring (wk :> string))
                :: ("first", Vbool (i = 0))
                :: env
              in
              List.iter (print_ast env ep) al)
            witnesses
      | _ -> ()
  in
  let print_foreach_event_witness_relation env al ((p, p_auth) as ep) =
    let related = List.sort_uniq compare (get_related p) in
    let events_witnesses =
      let l = ref [] in
      (let rec make_list = function
         | ic :: icl ->
             let c = pget conf base ic in
             List.iter
               (fun ((name, _, _, _, _, wl, _) as evt) ->
                 match Util.array_mem_witn conf base (get_iper p) wl with
                 | None -> ()
                 | Some wk -> (
                     match name with
                     | Event.Pevent _ -> l := (c, wk, evt) :: !l
                     | Event.Fevent _ ->
                         if get_sex c = Male then l := (c, wk, evt) :: !l))
               (Event.sorted_events conf base c);
             make_list icl
         | [] -> ()
       in
       make_list related);
      !l
    in
    (* On tri les témoins dans le même ordre que les évènements. *)
    let events_witnesses =
      Event.sort_events
        (fun (_, _, (name, _, _, _, _, _, _)) -> name)
        (fun (_, _, (_, date, _, _, _, _, _)) -> date)
        events_witnesses
    in
    List.iter
      (fun (p, wk, evt) ->
        if p_auth then
          let env = ("event_witness_relation", Vevent (p, evt)) :: env in
          let env =
            ( "event_witness_relation_kind",
              Vstring (wk : Adef.safe_string :> string) )
            :: env
          in
          List.iter (print_ast env ep) al)
      events_witnesses
  in
  let print_foreach_witness env al ep witness_kind = function
    | Vfam (_, fam, _, true) ->
        let _ =
          Array.fold_left
            (fun (i, first) (ip, wk) ->
              let p = pget conf base ip in
              (* TODO if witness_kind = Witness, we might want wk = "" *)
              let wks =
                if witness_kind = Witness && wk = Witness then ""
                else (Util.string_of_witness_kind conf (get_sex p) wk :> string)
              in
              let env =
                ("witness", Vind p) :: ("first", Vbool first)
                :: ("witness_kind", Vstring wks)
                :: env
              in
              if witness_kind = Witness || witness_kind = wk then (
                List.iter (print_ast env ep) al;
                (i + 1, false))
              else (i, first))
            (0, true)
            (get_marriage_witnesses fam)
        in
        ()
    | _ -> ()
  in

  let print_foreach_witness_relation env al ((p, _) as ep) =
    let l =
      let related = List.sort_uniq compare (get_related p) in
      let l = ref [] in
      List.iter
        (fun ic ->
          let c = pget conf base ic in
          (* TODO WHY: only on Male? probably bugged on same sex or neuter couples *)
          if get_sex c = Male then
            Array.iter
              (fun ifam ->
                let fam = foi base ifam in
                if Array.mem (get_iper p) (get_witnesses fam) then
                  l := (ifam, fam) :: !l)
              (get_family (pget conf base ic)))
        related;
      !l
    in
    let l =
      List.sort
        (fun (_, fam1) (_, fam2) ->
          match
            ( Date.od_of_cdate (get_marriage fam1),
              Date.od_of_cdate (get_marriage fam2) )
          with
          | Some d1, Some d2 -> Date.compare_date d1 d2
          | _ -> 0)
        l
    in
    List.iter
      (fun (ifam, fam) ->
        let ifath = get_father fam in
        let imoth = get_mother fam in
        let cpl = (ifath, imoth, imoth) in
        let m_auth =
          authorized_age conf base (pget conf base ifath)
          && authorized_age conf base (pget conf base imoth)
        in
        if m_auth then
          let env = ("fam", Vfam (ifam, fam, cpl, true)) :: env in
          List.iter (print_ast env ep) al)
      l
  in

  let print_foreach_family env al ini_ep (p, _) =
    match get_env "p_link" env with
    | Vbool _ ->
        let conf =
          match get_env "baseprefix" env with
          | Vstring baseprefix -> { conf with command = baseprefix }
          | _ -> conf
        in
        List.fold_left
          (fun (prev, i) (ifam, fam, (ifath, imoth, spouse), baseprefix, _) ->
            let cpl = (ifath, imoth, get_iper spouse) in
            let vfam = Vfam (ifam, fam, cpl, true) in
            let env = ("#loop", Vint 0) :: env in
            let env = ("fam_link", vfam) :: env in
            let env = ("f_link", Vbool true) :: env in
            let env = ("is_link", Vbool true) :: env in
            let env = ("baseprefix", Vstring baseprefix) :: env in
            let env = ("family_cnt", Vint (i + 1)) :: env in
            let env =
              match prev with
              | Some vfam -> ("prev_fam", vfam) :: env
              | None -> env
            in
            List.iter (print_ast env ini_ep) al;
            (Some vfam, i + 1))
          (None, 0)
          (!GWPARAM_ITL.get_families conf base p)
        |> ignore
    | _ ->
        (if Array.length (get_family p) > 0 then
         let rec loop prev i =
           if i = Array.length (get_family p) then ()
           else
             let ifam = (get_family p).(i) in
             let fam = foi base ifam in
             let ifath = get_father fam in
             let imoth = get_mother fam in
             let ispouse = Gutil.spouse (get_iper p) fam in
             let cpl = (ifath, imoth, ispouse) in
             let m_auth =
               authorized_age conf base (pget conf base ifath)
               && authorized_age conf base (pget conf base imoth)
             in
             let vfam = Vfam (ifam, fam, cpl, m_auth) in
             let env = ("#loop", Vint 0) :: env in
             let env = ("fam", vfam) :: env in
             let env = ("family_cnt", Vint (i + 1)) :: env in
             let env =
               match prev with
               | Some vfam -> ("prev_fam", vfam) :: env
               | None -> env
             in
             List.iter (print_ast env ini_ep) al;
             loop (Some vfam) (i + 1)
         in
         loop None 0);
        List.fold_left
          (fun (prev, i) (ifam, fam, (ifath, imoth, sp), baseprefix, can_merge) ->
            if can_merge then (None, i)
            else
              let cpl = (ifath, imoth, get_iper sp) in
              let vfam = Vfam (ifam, fam, cpl, true) in
              let env = ("#loop", Vint 0) :: env in
              let env = ("fam_link", vfam) :: env in
              let env = ("f_link", Vbool true) :: env in
              let env = ("is_link", Vbool true) :: env in
              let env = ("baseprefix", Vstring baseprefix) :: env in
              let env = ("family_cnt", Vint (i + 1)) :: env in
              let env =
                match prev with
                | Some vfam -> ("prev_fam", vfam) :: env
                | None -> env
              in
              List.iter (print_ast env ini_ep) al;
              (Some vfam, i + 1))
          (None, 0)
          (!GWPARAM_ITL.get_families conf base p)
        |> ignore
  in

  let print_foreach_first_name_alias env al ((p, p_auth) as ep) =
    if (not p_auth) && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first s ->
          let env = ("first_name_alias", Vstring (sou base s)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (get_first_names_aliases p)
  in

  let print_foreach_cousin_path env el al ((p, _) as ep) in_or_less =
    let get_level_info conf env el ep =
      let v1_v2 =
        match get_env "v1_v2" env with Vcous_level _ -> true | _ -> false
      in
      let l1, l2 =
        match el with
        | [ [ e1 ]; [ e2 ] ] ->
            (eval_int_expr env ep e1, eval_int_expr env ep e2)
        | [ [ e1 ] ] -> (eval_int_expr env ep e1, 0)
        | [] when v1_v2 -> (
            match get_env "v1_v2" env with
            | Vcous_level (v1, v2) -> (!v1, !v2)
            | _ -> (0, 0))
        | [] -> (
            match (p_getenv conf.env "v1", p_getenv conf.env "v2") with
            | Some v1, Some v2 -> (
                match (int_of_string_opt v1, int_of_string_opt v2) with
                | Some v1, Some v2 -> (v1, v2)
                | _, _ -> raise Not_found)
            | Some v1, _ -> (
                match int_of_string_opt v1 with
                | Some v1 -> (v1, 0)
                | _ -> raise Not_found)
            | _, Some v2 -> (
                match int_of_string_opt v2 with
                | Some v2 -> (0, v2)
                | _ -> raise Not_found)
            | _ -> (0, 0))
        | _ -> (0, 0)
      in
      let level = abs l1 - l2 in
      (level, l1, l2)
    in
    let level, l1, l2 = get_level_info conf env el ep in
    let l =
      Cousins.cousins_l1_l2_aux conf base (string_of_int l1) (string_of_int l2)
        p
    in
    match l with
    | Some l ->
        print_foreach_path_aux conf base in_or_less level env al ep
          (Cousins.cousins_fold l)
    | None -> !GWPARAM.syslog `LOG_WARNING "Empty cousins list"
  in

  let print_foreach_cousin_level env al ((_, _) as ep) =
    let max_level =
      match get_env "max_cous_level" env with Vint n -> n | _ -> 0
    in
    let rec loop i =
      if i > max_level then ()
      else
        let env = ("level", Vint i) :: env in
        List.iter (print_ast env ep) al;
        loop (succ i)
    in
    loop 0
  in

  let print_foreach_nobility_title env al ((p, p_auth) as ep) =
    if p_auth then
      let titles = nobility_titles_list conf base p in
      Mutil.list_iter_first
        (fun first x ->
          let env = ("nobility_title", Vtitle (p, x)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        titles
  in
  let print_foreach_nob_title env al ((p, p_auth) as ep) =
    if p_auth then
      let titles = nobility_titles_list conf base p in
      Mutil.list_iter_first
        (fun first x ->
          let env = ("nob_title", Vtitle (p, x)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        titles
  in
  let print_foreach_parent env al ((a, _) as ep) =
    match get_parents a with
    | Some ifam ->
        let cpl = foi base ifam in
        Array.iter
          (fun iper ->
            let p = pget conf base iper in
            let env = ("parent", Vind p) :: env in
            List.iter (print_ast env ep) al)
          (get_parent_array cpl)
    | None -> ()
  in
  let print_foreach_qualifier env al ((p, p_auth) as ep) =
    if (not p_auth) && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first nn ->
          let env = ("qualifier", Vstring (sou base nn)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (get_qualifiers p)
  in
  let print_foreach_relation env al ((p, p_auth) as ep) =
    if p_auth then
      Mutil.list_iter_first
        (fun first r ->
          let env = ("rel", Vrel (r, None)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (get_rparents p)
  in
  let print_foreach_related env al ((p, p_auth) as ep) =
    if p_auth then
      let l =
        let l = List.sort_uniq compare (get_related p) in
        List.fold_left
          (fun l ic ->
            let c = pget conf base ic in
            let rec loop l = function
              | r :: rl -> (
                  match r.r_fath with
                  | Some ip when ip = get_iper p -> loop ((c, r) :: l) rl
                  | Some _ | None -> (
                      match r.r_moth with
                      | Some ip when ip = get_iper p -> loop ((c, r) :: l) rl
                      | Some _ | None -> loop l rl))
              | [] -> l
            in
            loop l (get_rparents c))
          [] l
      in
      let l =
        (* TODO don't query db in sort *)
        List.sort
          (fun (c1, _) (c2, _) ->
            let d1 =
              match Date.od_of_cdate (get_baptism c1) with
              | None -> Date.od_of_cdate (get_birth c1)
              | x -> x
            in
            let d2 =
              match Date.od_of_cdate (get_baptism c2) with
              | None -> Date.od_of_cdate (get_birth c2)
              | x -> x
            in
            match (d1, d2) with
            | Some d1, Some d2 -> Date.compare_date d1 d2
            | _ -> -1)
          (List.rev l)
      in
      List.iter
        (fun (c, r) ->
          let env = ("rel", Vrel (r, Some c)) :: env in
          List.iter (print_ast env ep) al)
        l
  in
  let print_foreach_sorted_list_item env al ep listname =
    let l =
      match get_env listname env with
      | Vslist l -> SortedList.elements !l
      | _ -> []
    in
    let rec loop prev_item = function
      | [] -> ()
      | _ :: sll as gsll ->
          let item = Vslistlm gsll in
          let env = ("item", item) :: ("prev_item", prev_item) :: env in
          List.iter (print_ast env ep) al;
          loop item sll
    in
    loop (Vslistlm []) l
  in
  let print_foreach_source env al ((p, p_auth) as ep) =
    let rec insert_loop typ src = function
      | (typ1, src1) :: srcl ->
          if src = src1 then (typ1 ^ ", " ^ typ, src1) :: srcl
          else (typ1, src1) :: insert_loop typ src srcl
      | [] -> [ (typ, src) ]
    in
    let insert typ src srcl =
      if src = "" then srcl else insert_loop (Util.translate_eval typ) src srcl
    in
    let srcl =
      if p_auth then
        let srcl = [] in
        let srcl =
          insert
            (transl_nth conf "person/persons" 0)
            (sou base (get_psources p))
            srcl
        in
        let srcl =
          insert (transl_nth conf "birth" 0) (sou base (get_birth_src p)) srcl
        in
        let srcl =
          insert
            (transl_nth conf "baptism" 0)
            (sou base (get_baptism_src p))
            srcl
        in
        let srcl, _ =
          Array.fold_left
            (fun (srcl, i) ifam ->
              let fam = foi base ifam in
              let isp = Gutil.spouse (get_iper p) fam in
              let sp = poi base isp in
              (* On sait que p_auth vaut vrai. *)
              let m_auth = authorized_age conf base sp in
              if m_auth then
                let lab =
                  if Array.length (get_family p) = 1 then ""
                  else " " ^ string_of_int i
                in
                let srcl =
                  let src_typ = transl_nth conf "marriage/marriages" 0 in
                  insert (src_typ ^ lab) (sou base (get_marriage_src fam)) srcl
                in
                let src_typ = transl_nth conf "family/families" 0 in
                ( insert (src_typ ^ lab) (sou base (get_fsources fam)) srcl,
                  i + 1 )
              else (srcl, i + 1))
            (srcl, 1) (get_family p)
        in
        let srcl =
          insert (transl_nth conf "death" 0) (sou base (get_death_src p)) srcl
        in
        let buri_crem_lex =
          match get_burial p with
          | Cremated _cdate -> "cremation"
          | Buried _cdate -> "burial"
          | UnknownBurial -> "burial" (* TODOWHY what should we print here *)
        in
        insert
          (transl_nth conf buri_crem_lex 0)
          (sou base (get_burial_src p))
          srcl
      else []
    in
    (* Affiche les sources et met à jour les variables "first" et "last". *)
    let rec loop first = function
      | (src_typ, src) :: srcl ->
          let env =
            ("first", Vbool first)
            :: ("last", Vbool (srcl = []))
            :: ("src_typ", Vstring src_typ)
            :: ("src", Vstring src) :: env
          in
          List.iter (print_ast env ep) al;
          loop false srcl
      | [] -> ()
    in
    loop true srcl
  in
  let print_foreach_surname_alias env al ((p, p_auth) as ep) =
    if (not p_auth) && is_hide_names conf p then ()
    else
      Mutil.list_iter_first
        (fun first s ->
          let env = ("surname_alias", Vstring (sou base s)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (get_surnames_aliases p)
  in
  (* carrousel *)
  let print_foreach_img_in_carrousel env al ((p, _p_auth) as ep) old =
    let l =
      let l =
        (if old then Image.get_carrousel_old_imgs else Image.get_carrousel_imgs)
          conf base p
      in
      List.sort (fun (a, _, _, _) (b, _, _, _) -> String.compare a b) l
    in
    let rec loop first cnt = function
      | [] -> ()
      | (name, url, src, note) :: l ->
          let env =
            ("carrousel_img", Vstring (Filename.basename name))
            :: ("carrousel_img_src", Vstring src)
            :: ("carrousel_img_note", Vstring note)
            :: ("first", Vbool first)
            :: ("last", Vbool (l = []))
            :: ("url", Vstring url) :: ("img_cnt", Vint cnt) :: env
          in
          List.iter (print_ast env ep) al;
          loop false (cnt + 1) l
    in
    loop true 1 l
  in
  let print_simple_foreach env el al ini_ep ep efam loc = function
    | "alias" -> print_foreach_alias env al ep
    | "ancestor" | "ascendant" -> print_foreach_ascendant env al ep
    | "ancestor_level" | "ascendant_level" ->
        print_foreach_ascendant_level env el al ep
    | "ancestor_at_level" | "ascendant_at_level" ->
        print_foreach_ascendant_at_level env al ep
    | "ancestor_surname" -> print_foreach_anc_surn env el al loc ep
    | "ancestor_tree_line" -> print_foreach_ancestor_tree env el al ep
    | "cell" -> print_foreach_cell env al ep
    | "child" -> print_foreach_child env al ep efam
    | "path" | "cous_path" -> print_foreach_cousin_path env el al ep false
    | "path_at_level" | "cous_path_at_level" ->
        print_foreach_cousin_path env el al ep true
    | "cousin_level" -> print_foreach_cousin_level env al ep
    | "descendant" -> print_foreach_descendant env al ep false
    | "descendant_cnt" -> print_foreach_descendant env al ep true
    | "descendant_level" -> print_foreach_descendant_level env al ep
    | "event" -> print_foreach_event env al ep
    | "family" -> print_foreach_family env al ini_ep ep
    | "first_name_alias" -> print_foreach_first_name_alias env al ep
    | "img_in_carrousel" -> print_foreach_img_in_carrousel env al ep false
    | "img_in_carrousel_old" -> print_foreach_img_in_carrousel env al ep true
    | "nobility_title" -> print_foreach_nobility_title env al ep
    | "nob_title" -> print_foreach_nob_title env al ep
    | "parent" -> print_foreach_parent env al ep
    | "qualifier" -> print_foreach_qualifier env al ep
    | "related" -> print_foreach_related env al ep
    | "relation" -> print_foreach_relation env al ep
    | "sorted_list_item" -> print_foreach_sorted_list_item env al ep "list"
    | "sorted_listb_item" -> print_foreach_sorted_list_item env al ep "listb"
    | "sorted_listc_item" -> print_foreach_sorted_list_item env al ep "listc"
    | "sorted_listd_item" -> print_foreach_sorted_list_item env al ep "listd"
    | "sorted_liste_item" -> print_foreach_sorted_list_item env al ep "liste"
    | "source" -> print_foreach_source env al ep
    | "surname_alias" -> print_foreach_surname_alias env al ep
    | "witness" -> print_foreach_witness env al ep Witness efam
    | "witness_godparent" ->
        print_foreach_witness env al ep Witness_GodParent efam
    | "witness_civilofficer" ->
        print_foreach_witness env al ep Witness_CivilOfficer efam
    | "witness_religiousofficer" ->
        print_foreach_witness env al ep Witness_ReligiousOfficer efam
    | "witness_informant" ->
        print_foreach_witness env al ep Witness_Informant efam
    | "witness_attending" ->
        print_foreach_witness env al ep Witness_Attending efam
    | "witness_mentioned" ->
        print_foreach_witness env al ep Witness_Mentioned efam
    | "witness_other" -> print_foreach_witness env al ep Witness_Other efam
    | "baptism_witness" ->
        print_foreach_epers_event_witness env al ep Epers_Baptism
    | "birth_witness" -> print_foreach_epers_event_witness env al ep Epers_Birth
    | "burial_witness" ->
        print_foreach_epers_event_witness env al ep Epers_Burial
    | "cremation_witness" ->
        print_foreach_epers_event_witness env al ep Epers_Cremation
    | "death_witness" -> print_foreach_epers_event_witness env al ep Epers_Death
    | "event_witness" -> print_foreach_event_witness env al ep
    | "event_witness_relation" -> print_foreach_event_witness_relation env al ep
    | "witness_relation" -> print_foreach_witness_relation env al ep
    | _ -> raise Not_found
  in
  let print_foreach env ini_ep loc s sl ell al =
    let rec loop env ((a, _) as ep) efam = function
      | [ s ] -> print_simple_foreach env ell al ini_ep ep efam loc s
      | "ancestor" :: sl -> (
          let ip_ifamo =
            match get_env "ancestor" env with
            | Vanc (GP_person (_, ip, ifamo)) -> Some (ip, ifamo)
            | Vanc (GP_same (_, _, ip)) -> Some (ip, None)
            | _ -> None
          in
          match ip_ifamo with
          | Some (ip, ifamo) ->
              let ep = make_ep conf base ip in
              let efam =
                match ifamo with
                | Some ifam ->
                    let f, c, a = make_efam conf base ip ifam in
                    Vfam (ifam, f, c, a)
                | None -> efam
              in
              loop env ep efam sl
          | None -> raise Not_found)
      | "child" :: sl -> (
          match get_env "child" env with
          | Vind p ->
              let auth = authorized_age conf base p in
              let ep = (p, auth) in
              loop env ep efam sl
          | _ -> (
              match get_env "child_link" env with
              | Vind p ->
                  let env = ("p_link", Vbool true) :: env in
                  let env = ("f_link", Vbool true) :: env in
                  let auth = authorized_age conf base p in
                  let ep = (p, auth) in
                  loop env ep efam sl
              | _ -> raise Not_found))
      | "father" :: sl -> (
          match get_parents a with
          | Some ifam ->
              let cpl = foi base ifam in
              let ((_, p_auth) as ep) = make_ep conf base (get_father cpl) in
              let ifath = get_father cpl in
              let cpl = (ifath, get_mother cpl, ifath) in
              let m_auth =
                p_auth && authorized_age conf base (pget conf base ifath)
              in
              let efam = Vfam (ifam, foi base ifam, cpl, m_auth) in
              loop env ep efam sl
          | None -> (
              let conf =
                match get_env "baseprefix" env with
                | Vstring baseprefix -> { conf with command = baseprefix }
                | _ -> conf
              in
              match !GWPARAM_ITL.get_father' conf base (get_iper a) with
              | Some (baseprefix, ep, ifam, fam, cpl) ->
                  let efam = Vfam (ifam, fam, cpl, true) in
                  let env = ("p_link", Vbool true) :: env in
                  let env = ("f_link", Vbool true) :: env in
                  let env = ("baseprefix", Vstring baseprefix) :: env in
                  loop env ep efam sl
              | None -> warning_use_has_parents_before_parent loc "father" ()))
      | "mother" :: sl -> (
          match get_parents a with
          | Some ifam ->
              let cpl = foi base ifam in
              let ((_, p_auth) as ep) = make_ep conf base (get_mother cpl) in
              let ifath = get_father cpl in
              let cpl = (ifath, get_mother cpl, ifath) in
              let m_auth =
                p_auth && authorized_age conf base (pget conf base ifath)
              in
              let efam = Vfam (ifam, foi base ifam, cpl, m_auth) in
              loop env ep efam sl
          | None -> (
              match !GWPARAM_ITL.get_mother' conf base (get_iper a) with
              | Some (baseprefix, ep, ifam, fam, cpl) ->
                  let efam = Vfam (ifam, fam, cpl, true) in
                  let env = ("p_link", Vbool true) :: env in
                  let env = ("f_link", Vbool true) :: env in
                  let env = ("baseprefix", Vstring baseprefix) :: env in
                  loop env ep efam sl
              | None -> warning_use_has_parents_before_parent loc "mother" ()))
      | "self" :: sl -> loop env ep efam sl
      | "spouse" :: sl -> (
          match efam with
          | Vfam (_, _, (_, _, ip), _) ->
              let ep = make_ep conf base ip in
              loop env ep efam sl
          | _ -> (
              match get_env "fam_link" env with
              | Vfam (_, _, (_, _, ip), _) -> (
                  let baseprefix =
                    match get_env "baseprefix" env with
                    | Vstring baseprefix -> baseprefix
                    | _ -> conf.command
                  in
                  match !GWPARAM_ITL.get_person conf base baseprefix ip with
                  | Some (ep, baseprefix) ->
                      let env = ("p_link", Vbool true) :: env in
                      let env = ("baseprefix", Vstring baseprefix) :: env in
                      loop env ep efam sl
                  | None -> raise Not_found)
              | _ -> raise Not_found))
      | _ -> raise Not_found
    in
    let efam =
      match get_env "is_link" env with
      | Vbool _ -> get_env "fam_link" env
      | _ -> get_env "fam" env
    in
    loop env ini_ep efam (s :: sl)
  in
  print_foreach

let eval_predefined_apply conf env f vl =
  let vl = List.map (function VVstring s -> s | _ -> raise Not_found) vl in
  match (f, vl) with
  | "a_of_b", [ s1; s2 ] -> Util.translate_eval (transl_a_of_b conf s1 s2 s2)
  | "a_of_b2", [ s1; s2; s3 ] ->
      Util.translate_eval (transl_a_of_b conf s1 s2 s3)
  | "a_of_b_gr_eq_lev", [ s1; s2 ] ->
      Util.translate_eval (transl_a_of_gr_eq_gen_lev conf s1 s2 s2)
  | "add_in_sorted_list", sl -> (
      match get_env "list" env with
      | Vslist l ->
          l := SortedList.add sl !l;
          ""
      | _ -> raise Not_found)
  | "add_in_sorted_listb", sl -> (
      match get_env "listb" env with
      | Vslist l ->
          l := SortedList.add sl !l;
          ""
      | _ -> raise Not_found)
  | "add_in_sorted_listc", sl -> (
      match get_env "listc" env with
      | Vslist l ->
          l := SortedList.add sl !l;
          ""
      | _ -> raise Not_found)
  | "add_in_sorted_listd", sl -> (
      match get_env "listd" env with
      | Vslist l ->
          l := SortedList.add sl !l;
          ""
      | _ -> raise Not_found)
  | "add_in_sorted_liste", sl -> (
      match get_env "liste" env with
      | Vslist l ->
          l := SortedList.add sl !l;
          ""
      | _ -> raise Not_found)
  | "hexa", [ s ] -> Util.hexa_string s
  | "initial", [ s ] ->
      if String.length s = 0 then "" else String.sub s 0 (Utf8.next s 0)
  | "lazy_print", [ v ] -> (
      match get_env "lazy_print" env with
      | Vlazyp r ->
          r := Some v;
          ""
      | _ -> raise Not_found)
  | "min", sl -> (
      try
        let sl =
          List.map (fun s -> if s = "" then max_int else int_of_string s) sl
        in
        let m = List.fold_left min max_int sl in
        string_of_int m
      with Failure _ -> raise Not_found)
  | "max", sl -> (
      try
        let sl =
          List.map (fun s -> if s = "" then 0 - max_int else int_of_string s) sl
        in
        let m = List.fold_left max (-max_int) sl in
        string_of_int m
      with Failure _ -> raise Not_found)
  | "clean_html_tags", [ s ] -> Util.clean_html_tags s
  | "clean_comment_tags", [ s ] -> Util.clean_comment_tags s
  | "uri_encode", [ s ] -> Util.uri_encode s
  | "uri_decode", [ s ] -> Util.uri_decode s
  | _ -> raise Not_found

let gen_interp_templ ?(no_headers = false) menu title templ_fname conf base p =
  template_file := templ_fname ^ ".txt";
  let ep = (p, authorized_age conf base p) in
  let emal =
    match p_getint conf.env "v" with Some i -> i | None -> Cousins.mal
  in
  let env =
    let sosa_ref = Util.find_sosa_ref conf base in
    if sosa_ref <> None then SosaCache.build_sosa_ht conf base;
    let t_sosa =
      match sosa_ref with
      | Some p -> SosaCache.init_sosa_t conf base p
      | None -> None
    in
    let desc_level_table_l =
      let dlt () = make_desc_level_table conf base emal p in
      Lazy.from_fun dlt
    in
    let desc_level_table_m =
      let dlt () = make_desc_level_table conf base Cousins.mdl p in
      Lazy.from_fun dlt
    in
    let desc_level_table_l_save =
      let dlt () = make_desc_level_table conf base emal p in
      Lazy.from_fun dlt
    in
    let mal () =
      Vint (Cousins.max_ancestor_level conf base (get_iper p) (emal + 1))
    in
    (* Static max ancestor level *)
    let smal () =
      Vint (Cousins.max_ancestor_level conf base (get_iper p) Cousins.mal)
    in
    (* Sosa_ref max ancestor level *)
    let srmal () =
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
          Vint
            (Cousins.max_ancestor_level conf base (get_iper sosa_ref)
               Cousins.mal)
      | None -> Vint 0
    in
    let mcl () = Vint (Cousins.max_cousin_level conf) in
    (* Récupère le nombre maximal de niveaux de descendance en prenant en
       compte les liens inter-arbres (limité à 10 générations car
       problématique en terme de perf). *)
    let mdl () =
      Vint
        (max
           (max_descendant_level base desc_level_table_l)
           (!GWPARAM_ITL.max_descendant_level conf base (get_iper p) 10))
    in
    (* Static max descendant level *)
    let smdl () = Vint (max_descendant_level base desc_level_table_m) in
    let nldb () =
      let db = Gwdb.read_nldb base in
      let db = Notes.merge_possible_aliases conf db in
      Vnldb db
    in
    let all_gp () = Vallgp (get_all_generations conf base p) in
    [
      ("p", Vind p);
      ("p_auth", Vbool (authorized_age conf base p));
      ("count", Vcnt (ref 0));
      ("count1", Vcnt (ref 0));
      ("count2", Vcnt (ref 0));
      ("count3", Vcnt (ref 0));
      ("vars", Vvars (ref []));
      ("cousins", Vcousl (ref []));
      ("v1_v2", Vcous_level (ref 0, ref 0));
      ("list", Vslist (ref SortedList.empty));
      ("listb", Vslist (ref SortedList.empty));
      ("listc", Vslist (ref SortedList.empty));
      ("listd", Vslist (ref SortedList.empty));
      ("liste", Vslist (ref SortedList.empty));
      ("desc_mark", Vdmark (ref @@ Gwdb.dummy_marker Gwdb.dummy_iper false));
      ("lazy_print", Vlazyp (ref None));
      ("sosa", Vsosa (ref []));
      ("sosa_ref", Vsosa_ref sosa_ref);
      ("t_sosa", Vt_sosa t_sosa);
      ("max_anc_level", Vlazy (Lazy.from_fun mal));
      ("static_max_anc_level", Vlazy (Lazy.from_fun smal));
      ("sosa_ref_max_anc_level", Vlazy (Lazy.from_fun srmal));
      ("max_cous_level", Vlazy (Lazy.from_fun mcl));
      ("max_desc_level", Vlazy (Lazy.from_fun mdl));
      ("static_max_desc_level", Vlazy (Lazy.from_fun smdl));
      ("desc_level_table", Vdesclevtab desc_level_table_l);
      ("desc_level_table_save", Vdesclevtab desc_level_table_l_save);
      ("nldb", Vlazy (Lazy.from_fun nldb));
      ("all_gp", Vlazy (Lazy.from_fun all_gp));
    ]
  in
  if no_headers then
    Hutil.interp_no_header conf templ_fname
      {
        Templ.eval_var = eval_var conf base;
        Templ.eval_transl = eval_transl conf base;
        Templ.eval_predefined_apply = eval_predefined_apply conf;
        Templ.get_vother;
        Templ.set_vother;
        Templ.print_foreach = print_foreach conf base;
      }
      env ep
  else if menu then
    let size =
      match Util.open_etc_file conf templ_fname with
      | Some (ic, _) ->
          let fd = Unix.descr_of_in_channel ic in
          let stats = Unix.fstat fd in
          close_in ic;
          stats.Unix.st_size
      | None -> 0
    in
    if size = 0 then Hutil.header conf title
    else
      Hutil.interp_no_header conf templ_fname
        {
          Templ.eval_var = eval_var conf base;
          Templ.eval_transl = eval_transl conf base;
          Templ.eval_predefined_apply = eval_predefined_apply conf;
          Templ.get_vother;
          Templ.set_vother;
          Templ.print_foreach = print_foreach conf base;
        }
        env ep
  else
    Hutil.interp conf templ_fname
      {
        Templ.eval_var = eval_var conf base;
        Templ.eval_transl = eval_transl conf base;
        Templ.eval_predefined_apply = eval_predefined_apply conf;
        Templ.get_vother;
        Templ.set_vother;
        Templ.print_foreach = print_foreach conf base;
      }
      env ep

let interp_templ ?no_headers = gen_interp_templ ?no_headers false (fun _ -> ())
let interp_templ_with_menu = gen_interp_templ true

let interp_notempl_with_menu title templ_fname conf base p =
  (* On envoie le header car on n'est pas dans un template (exple: merge). *)
  Hutil.header_without_page_title conf title;
  gen_interp_templ true title templ_fname conf base p

(* Main *)

let print ?no_headers conf base p =
  let passwd =
    if conf.wizard || conf.friend then None
    else
      let src =
        match get_parents p with
        | Some ifam -> sou base (get_origin_file (foi base ifam))
        | None -> ""
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.base_env)
      with Not_found -> None
  in
  match passwd with
  | Some (src, passwd)
    when is_that_user_and_password conf.auth_scheme "" passwd = false ->
      Util.unauthorized conf src
  | Some _ | None -> interp_templ ?no_headers "perso" conf base p

let print_what_links conf base p =
  if authorized_age conf base p then (
    let key =
      let fn = Name.lower (sou base (get_first_name p)) in
      let sn = Name.lower (sou base (get_surname p)) in
      (fn, sn, get_occ p)
    in
    let db = Gwdb.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    let pgl = links_to_ind conf base db key in
    let title h =
      transl conf "linked pages" |> Utf8.capitalize_fst
      |> Output.print_sstring conf;
      Util.transl conf ":" |> Output.print_sstring conf;
      if h then Output.print_string conf (simple_person_text conf base p true)
      else (
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (commd conf);
        Output.print_string conf (acces conf base p);
        Output.print_sstring conf {|">|};
        Output.print_string conf (simple_person_text conf base p true);
        Output.print_sstring conf {|</a>|})
    in
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf true;
    NotesDisplay.print_linked_list conf base pgl;
    Hutil.trailer conf)
  else Hutil.incorrect_request conf
