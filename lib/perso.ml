(* Copyright (c) 1998-2007 INRIA *)

let round_2_dec x = floor ((x *. 100.0) +. 0.5) /. 100.0

let string_of_title ?(safe = false) ?(link = true) conf base
    (and_txt : Adef.safe_string) p (nth, name, title, places, dates) =
  let safe_html = if not safe then Util.safe_html else Adef.safe in
  let escape_html = if not safe then Util.escape_html else Adef.escaped in
  let tit, est = (Gwdb.sou base title, Gwdb.sou base (List.hd places)) in
  let acc = safe_html (tit ^ " " ^ est) in
  let href place s =
    if link then
      let href =
        let open Def in
        "m=TT&sm=S&t="
        ^<^ Mutil.encode (Gwdb.sou base title)
        ^^^ "&p="
        ^<^ Mutil.encode (Gwdb.sou base place)
      in
      Util.geneweb_link conf
        (href : Adef.encoded_string :> Adef.escaped_string)
        s
    else s
  in
  let acc = href (List.hd places) acc in
  let rec loop acc places =
    let acc =
      match places with
      | [] -> acc
      | [ _ ] ->
          let open Def in
          acc ^^^ " " ^<^ and_txt ^^^ Adef.safe " "
      | _ ->
          let open Def in
          acc ^>^ ", "
    in
    match places with
    | place :: places ->
        let est = safe_html (Gwdb.sou base place) in
        let acc =
          let open Def in
          acc ^^^ href place est
        in
        loop acc places
    | _ -> acc
  in
  let acc = loop acc (List.tl places) in
  let paren =
    match (nth, dates, name) with
    | n, _, _ when n > 0 -> true
    | _, _, Def.Tname _ -> true
    | _, (Some _, _) :: _, _ -> Util.authorized_age conf base p
    | _ -> false
  in
  let acc =
    if paren then
      let open Def in
      acc ^>^ " ("
    else acc
  in
  let first = nth <= 0 in
  let acc =
    if first then acc
    else
      let open Def in
      acc
      ^>^
      if nth >= 100 then string_of_int nth else Util.transl_nth conf "nth" nth
  in
  let acc, first =
    match name with
    | Def.Tname n ->
        let acc =
          if not first then
            let open Def in
            acc ^>^ " ,"
          else acc
        in
        let open Def in
        (acc ^^^ (Gwdb.sou base n |> escape_html :> Adef.safe_string), false)
    | _ -> (acc, first)
  in
  let acc =
    if Util.authorized_age conf base p && dates <> [ (None, None) ] then
      fst
      @@ List.fold_left
           (fun (acc, first) (date_start, date_end) ->
             let acc =
               if not first then
                 let open Def in
                 acc ^>^ ", "
               else acc
             in
             let acc =
               match date_start with
               | Some d ->
                   let open Def in
                   acc ^^^ DateDisplay.string_of_date conf d
               | None -> acc
             in
             let acc =
               match date_end with
               | Some (Date.Dgreg (d, _)) ->
                   if d.month <> 0 then
                     let open Def in
                     acc ^>^ " - "
                   else
                     let open Def in
                     acc ^>^ "-"
               | Some (Dtext _) ->
                   let open Def in
                   acc ^>^ " - "
               | _ -> acc
             in
             let acc =
               match date_end with
               | Some d ->
                   let open Def in
                   acc ^^^ DateDisplay.string_of_date conf d
               | None -> acc
             in
             (acc, false))
           (acc, first) dates
    else acc
  in
  if paren then
    let open Def in
    acc ^>^ ")"
  else acc

let name_equiv n1 n2 =
  Futil.eq_title_names Gwdb.eq_istr n1 n2
  || (n1 = Def.Tmain && n2 = Def.Tnone)
  || (n1 = Def.Tnone && n2 = Def.Tmain)

let nobility_titles_list conf base p =
  let titles =
    List.fold_right
      (fun t l ->
        let t_date_start = Date.od_of_cdate t.Def.t_date_start in
        let t_date_end = Date.od_of_cdate t.Def.t_date_end in
        match l with
        | (nth, name, title, place, dates) :: rl
          when (not conf.Config.is_rtl) && nth = t.Def.t_nth
               && name_equiv name t.Def.t_name
               && Gwdb.eq_istr title t.Def.t_ident
               && Gwdb.eq_istr place t.Def.t_place ->
            (nth, name, title, place, (t_date_start, t_date_end) :: dates) :: rl
        | _ ->
            ( t.Def.t_nth,
              t.Def.t_name,
              t.Def.t_ident,
              t.Def.t_place,
              [ (t_date_start, t_date_end) ] )
            :: l)
      (Util.nobtit conf base p) []
  in
  List.fold_right
    (fun (t_nth, t_name, t_ident, t_place, t_dates) l ->
      match l with
      | (nth, name, title, places, dates) :: rl
        when (not conf.Config.is_rtl) && nth = t_nth && name_equiv name t_name
             && Gwdb.eq_istr title t_ident && dates = t_dates ->
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
  let fn = Gwdb.sou base (Gwdb.get_first_name p) in
  let sn = Gwdb.sou base (Gwdb.get_surname p) in
  let occ = Gwdb.get_occ p in
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
      let is = Util.index_of_sex (Gwdb.get_sex p) in
      match Gwdb.get_death p with
      | Def.Death (dr, _) -> (
          match dr with
          | Def.Unspecified -> Util.transl_nth conf "died" is |> Adef.safe
          | Def.Murdered -> Util.transl_nth conf "murdered" is |> Adef.safe
          | Def.Killed ->
              Util.transl_nth conf "killed (in action)" is |> Adef.safe
          | Def.Executed ->
              Util.transl_nth conf "executed (legally killed)" is |> Adef.safe
          | Def.Disappeared ->
              Util.transl_nth conf "disappeared" is |> Adef.safe)
      | Def.DeadYoung -> Util.transl_nth conf "died young" is |> Adef.safe
      | Def.DeadDontKnowWhen -> Util.transl_nth conf "died" is |> Adef.safe
      | Def.NotDead | Def.DontKnowIfDead | Def.OfCourseDead -> "" |> Adef.safe
    else "" |> Adef.safe
  in
  let on_death_date =
    match (p_auth, Gwdb.get_death p) with
    | true, Def.Death (_, d) -> (
        let d = Date.date_of_cdate d in
        match List.assoc_opt "long_date" conf.Config.base_env with
        | Some "yes" ->
            let open Def in
            DateDisplay.string_of_ondate ~link:false conf d
            ^>^ DateDisplay.get_wday conf d
        | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
    | _ -> "" |> Adef.safe
  in
  let open Def in
  died ^^^ " " ^<^ on_death_date

let get_baptism_text conf p p_auth =
  let baptized =
    if p_auth then
      Gwdb.get_sex p |> Util.index_of_sex
      |> Util.transl_nth conf "baptized"
      |> Adef.safe
    else "" |> Adef.safe
  in
  let on_baptism_date =
    match (p_auth, Date.od_of_cdate (Gwdb.get_baptism p)) with
    | true, Some d -> (
        match List.assoc_opt "long_date" conf.Config.base_env with
        | Some "yes" ->
            let open Def in
            DateDisplay.string_of_ondate ~link:false conf d
            ^>^ DateDisplay.get_wday conf d
        | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
    | _ -> "" |> Adef.safe
  in
  let open Def in
  baptized ^^^ " " ^<^ on_baptism_date

let get_birth_text conf p p_auth =
  let born =
    if p_auth then
      Gwdb.get_sex p |> Util.index_of_sex
      |> Util.transl_nth conf "born"
      |> Adef.safe
    else "" |> Adef.safe
  in
  let on_birth_date =
    match (p_auth, Date.od_of_cdate (Gwdb.get_birth p)) with
    | true, Some d -> (
        match List.assoc_opt "long_date" conf.Config.base_env with
        | Some "yes" ->
            let open Def in
            DateDisplay.string_of_ondate ~link:false conf d
            ^>^ DateDisplay.get_wday conf d
        | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
    | _ -> "" |> Adef.safe
  in
  let open Def in
  born ^^^ " " ^<^ on_birth_date

let get_marriage_date_text conf fam p_auth =
  match (p_auth, Date.od_of_cdate (Gwdb.get_marriage fam)) with
  | true, Some d -> (
      match List.assoc_opt "long_date" conf.Config.base_env with
      | Some "yes" ->
          let open Def in
          DateDisplay.string_of_ondate ~link:false conf d
          ^>^ DateDisplay.get_wday conf d
      | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
  | _ -> "" |> Adef.safe

let get_burial_text conf p p_auth =
  let buried =
    if p_auth then
      Gwdb.get_sex p |> Util.index_of_sex
      |> Util.transl_nth conf "buried"
      |> Adef.safe
    else "" |> Adef.safe
  in
  let on_burial_date =
    match Gwdb.get_burial p with
    | Def.Buried cod -> (
        match (p_auth, Date.od_of_cdate cod) with
        | true, Some d -> (
            match List.assoc_opt "long_date" conf.Config.base_env with
            | Some "yes" ->
                let open Def in
                DateDisplay.string_of_ondate ~link:false conf d
                ^>^ DateDisplay.get_wday conf d
            | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
        | _ -> "" |> Adef.safe)
    | Def.UnknownBurial | Def.Cremated _ -> "" |> Adef.safe
  in
  let open Def in
  buried ^^^ " " ^<^ on_burial_date

let get_cremation_text conf p p_auth =
  let cremated =
    if p_auth then
      Gwdb.get_sex p |> Util.index_of_sex
      |> Util.transl_nth conf "cremated"
      |> Adef.safe
    else "" |> Adef.safe
  in
  let on_cremation_date =
    match Gwdb.get_burial p with
    | Def.Cremated cod -> (
        match (p_auth, Date.od_of_cdate cod) with
        | true, Some d -> (
            match List.assoc_opt "long_date" conf.Config.base_env with
            | Some "yes" ->
                let open Def in
                DateDisplay.string_of_ondate ~link:false conf d
                ^>^ DateDisplay.get_wday conf d
            | Some _ | None -> DateDisplay.string_of_ondate ~link:false conf d)
        | _ -> "" |> Adef.safe)
    | Def.UnknownBurial | Def.Buried _ -> "" |> Adef.safe
  in
  let open Def in
  cremated ^^^ " " ^<^ on_cremation_date

let limit_desc conf =
  match
    Option.map int_of_string
    @@ List.assoc_opt "max_desc_level" conf.Config.base_env
  with
  | Some x -> max 1 x
  | None -> 12

let infinite = 10000

let make_desc_level_table conf base max_level p =
  let line =
    match Util.p_getenv conf.Config.env "t" with
    | Some "M" -> Def.Male
    | Some "F" -> Def.Female
    | Some _ | None -> Def.Neuter
  in
  (* the table 'levt' may be not necessary, since I added 'flevt'; kept
     because '%max_desc_level;' is still used... *)
  let levt = Gwdb.iper_marker (Gwdb.ipers base) infinite in
  let flevt = Gwdb.ifam_marker (Gwdb.ifams base) infinite in
  let get = Util.pget conf base in
  let ini_ip = Gwdb.get_iper p in
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
                    | Def.Male ->
                        Gwdb.get_sex (Util.pget conf base ip) <> Def.Female
                    | Def.Female ->
                        Gwdb.get_sex (Util.pget conf base ip) <> Def.Male
                    | Def.Neuter -> true
                in
                if down then
                  Array.fold_left
                    (fun ipl ifam ->
                      if not (Gwdb.Marker.get flevt ifam <= lev) then
                        Gwdb.Marker.set flevt ifam lev;
                      let ipa = Gwdb.get_children (Gwdb.foi base ifam) in
                      Array.fold_left (fun ipl ip -> ip :: ipl) ipl ipa)
                    ipl
                    (Gwdb.get_family (get ip))
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
  | GP_person of Sosa.t * Gwdb.iper * Gwdb.ifam option
  | GP_same of Sosa.t * Sosa.t * Gwdb.iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * Gwdb.iper

let next_generation conf base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
        match gp with
        | GP_person (n, ip, _) -> (
            let n_fath = Sosa.twice n in
            let n_moth = Sosa.inc n_fath 1 in
            let a = Util.pget conf base ip in
            match Gwdb.get_parents a with
            | Some ifam ->
                let cpl = Gwdb.foi base ifam in
                GP_person (n_fath, Gwdb.get_father cpl, Some ifam)
                :: GP_person (n_moth, Gwdb.get_mother cpl, Some ifam)
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
    match Gwdb.get_parents (Util.pget conf base ip) with
    | Some ifam -> (
        match get_link all_gp (parent (Gwdb.foi base ifam)) with
        | Some (GP_person (n, _, _)) -> Sosa.to_string n
        | _ -> "")
    | None -> ""

let will_print = function
  | GP_person (_, _, _) -> true
  | GP_same (_, _, _) -> true
  | GP_interv _ | GP_missing _ -> false

let get_all_generations conf base p =
  let max_level =
    match Util.p_getint conf.Config.env "v" with Some v -> v | None -> 0
  in
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
  let gpll =
    get_generations 1 [] [ GP_person (Sosa.one, Gwdb.get_iper p, None) ]
  in
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

type cell =
  | Cell of Gwdb.person * Gwdb.ifam option * pos * bool * int * string
  | Empty

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
  let next_gen pol =
    List.fold_right
      (fun po list ->
        match po with
        | Empty -> Empty :: list
        | Cell (p, _, _, _, _, base_prefix) -> (
            match Gwdb.get_parents p with
            | Some ifam -> (
                let cpl = Gwdb.foi base ifam in
                let fath =
                  let p = Util.pget conf base (Gwdb.get_father cpl) in
                  if not @@ Util.is_empty_name p then Some p else None
                in
                let moth =
                  let p = Util.pget conf base (Gwdb.get_mother cpl) in
                  if not @@ Util.is_empty_name p then Some p else None
                in
                let fo = Some ifam in
                let base_prefix = conf.Config.bname in
                match (fath, moth) with
                | Some f, Some m ->
                    Cell (f, fo, Left, true, 1, base_prefix)
                    :: Cell (m, fo, Right, true, 1, base_prefix)
                    :: list
                | Some f, None ->
                    Cell (f, fo, Alone, true, 1, base_prefix) :: list
                | None, Some m ->
                    Cell (m, fo, Alone, true, 1, base_prefix) :: list
                | None, None -> Empty :: list)
            | None -> (
                match
                  !GWPARAM_ITL.tree_generation_list conf base base_prefix p
                with
                | Some (fath, if1, base_prefix1), Some (moth, if2, base_prefix2)
                  ->
                    Cell (fath, Some if1, Left, true, 1, base_prefix1)
                    :: Cell (moth, Some if2, Right, true, 1, base_prefix2)
                    :: list
                | Some (fath, ifam, base_prefix), None ->
                    Cell (fath, Some ifam, Alone, true, 1, base_prefix) :: list
                | None, Some (moth, ifam, base_prefix) ->
                    Cell (moth, Some ifam, Alone, true, 1, base_prefix) :: list
                | None, None -> Empty :: list)))
      pol []
  in
  let gen =
    let rec loop i gen list =
      if i = 0 then gen :: list else loop (i - 1) (next_gen gen) (gen :: list)
    in
    loop (gv - 1) [ Cell (p, None, Center, true, 1, conf.Config.bname) ] []
  in
  enrich_tree gen

(* Ancestors surnames list *)

let get_date_place conf base auth_for_all_anc p =
  if auth_for_all_anc || Util.authorized_age conf base p then
    let d1 =
      match Date.od_of_cdate (Gwdb.get_birth p) with
      | None -> Date.od_of_cdate (Gwdb.get_baptism p)
      | x -> x
    in
    let d1 =
      if d1 <> None then d1
      else
        Array.fold_left
          (fun d ifam ->
            if d <> None then d
            else Date.od_of_cdate (Gwdb.get_marriage (Gwdb.foi base ifam)))
          d1 (Gwdb.get_family p)
    in
    let d2 =
      match Date.date_of_death (Gwdb.get_death p) with
      | Some d -> Some d
      | None -> (
          match Gwdb.get_burial p with
          | Def.Buried cod | Def.Cremated cod -> Date.od_of_cdate cod
          | Def.UnknownBurial -> None)
    in
    let auth_for_all_anc =
      if auth_for_all_anc then true
      else
        match d2 with
        | Some (Dgreg (d, _)) ->
            let a = Date.time_elapsed d conf.Config.today in
            Util.strictly_after_private_years conf a
        | _ -> false
    in
    let pl =
      let pl = "" in
      let pl =
        if pl <> "" then pl else Gwdb.sou base (Gwdb.get_birth_place p)
      in
      let pl =
        if pl <> "" then pl else Gwdb.sou base (Gwdb.get_baptism_place p)
      in
      let pl =
        if pl <> "" then pl else Gwdb.sou base (Gwdb.get_death_place p)
      in
      let pl =
        if pl <> "" then pl else Gwdb.sou base (Gwdb.get_burial_place p)
      in
      if pl <> "" then pl
      else
        Array.fold_left
          (fun pl ifam ->
            if pl <> "" then pl
            else Gwdb.sou base (Gwdb.get_marriage_place (Gwdb.foi base ifam)))
          pl (Gwdb.get_family p)
    in
    ((d1, d2, pl), auth_for_all_anc)
  else ((None, None, ""), false)

(* duplications proposed for merging *)

type dup =
  | DupFam of Gwdb.ifam * Gwdb.ifam
  | DupInd of Gwdb.iper * Gwdb.iper
  | NoDup

type excl_dup = (Gwdb.iper * Gwdb.iper) list * (Gwdb.ifam * Gwdb.ifam) list

let gen_excluded_possible_duplications conf s i_of_string =
  match Util.p_getenv conf.Config.env s with
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
  ( gen_excluded_possible_duplications conf "iexcl" Gwdb.iper_of_string,
    gen_excluded_possible_duplications conf "fexcl" Gwdb.ifam_of_string )

let first_possible_duplication_children iexcl len child eq =
  let rec loop i =
    if i = len then NoDup
    else
      let c1 = child i in
      let rec loop' j =
        if j = len then loop (i + 1)
        else
          let c2 = child j in
          let ic1 = Gwdb.get_iper c1 in
          let ic2 = Gwdb.get_iper c2 in
          if List.mem (ic1, ic2) iexcl then loop' (j + 1)
          else if eq (Gwdb.get_first_name c1) (Gwdb.get_first_name c2) then
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
          let s = Name.lower @@ Gwdb.sou base i in
          cache := (i, s) :: !cache;
          s
  in
  let eq i1 i2 = str i1 = str i2 in
  let p = Gwdb.poi base ip in
  match Gwdb.get_family p with
  | [||] -> NoDup
  | [| ifam |] ->
      let children = Gwdb.get_children @@ Gwdb.foi base ifam in
      let len = Array.length children in
      if len < 2 then NoDup
      else
        let child i = Gwdb.poi base @@ Array.unsafe_get children i in
        first_possible_duplication_children iexcl len child eq
  | ifams ->
      let len = Array.length ifams in
      let fams = Array.make len None in
      let spouses = Array.make len None in
      let fam i =
        match Array.unsafe_get fams i with
        | Some f -> f
        | None ->
            let f = Gwdb.foi base @@ Array.unsafe_get ifams i in
            Array.unsafe_set fams i (Some f);
            f
      in
      let spouse i =
        match Array.unsafe_get spouses i with
        | Some sp -> sp
        | None ->
            let sp = Gwdb.poi base @@ Gutil.spouse ip @@ fam i in
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
                if Gwdb.get_iper sp1 = Gwdb.get_iper sp2 then
                  let ifam1 = Array.unsafe_get ifams i in
                  let ifam2 = Array.unsafe_get ifams j in
                  if not (List.mem (ifam2, ifam2) fexcl) then
                    DupFam (ifam1, ifam2)
                  else loop' (j + 1)
                else
                  let isp1 = Gwdb.get_iper sp1 in
                  let isp2 = Gwdb.get_iper sp2 in
                  if List.mem (isp1, isp2) iexcl then loop' (j + 1)
                  else if
                    eq (Gwdb.get_first_name sp1) (Gwdb.get_first_name sp2)
                    && eq (Gwdb.get_surname sp1) (Gwdb.get_surname sp2)
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
          @@ Array.init len (fun i -> Gwdb.get_children @@ fam i)
        in
        let len = Array.length ichildren in
        let children = Array.make len None in
        let child i =
          match Array.unsafe_get children i with
          | Some c -> c
          | None ->
              let c = Gwdb.poi base @@ Array.unsafe_get ichildren i in
              Array.unsafe_set children i (Some c);
              c
        in
        first_possible_duplication_children iexcl len child eq

let has_possible_duplications conf base p =
  let ip = Gwdb.get_iper p in
  let excl = excluded_possible_duplications conf in
  first_possible_duplication base ip excl <> NoDup

let merge_date_place conf base surn ((d1, d2, pl), auth) p =
  let (pd1, pd2, ppl), auth = get_date_place conf base auth p in
  let nd1 =
    if pd1 <> None then pd1
    else if Gwdb.eq_istr (Gwdb.get_surname p) surn then
      if pd2 <> None then pd2 else d1
    else None
  in
  let nd2 =
    if Gwdb.eq_istr (Gwdb.get_surname p) surn then
      if d2 <> None then d2
      else if d1 <> None then d1
      else if pd1 <> None then pd2
      else pd1
    else if pd2 <> None then pd2
    else if pd1 <> None then pd1
    else d1
  in
  let pl =
    if ppl <> "" then ppl
    else if Gwdb.eq_istr (Gwdb.get_surname p) surn then pl
    else ""
  in
  ((nd1, nd2, pl), auth)

let build_surnames_list conf base v p =
  let ht = Hashtbl.create 701 in
  let mark =
    let n =
      try int_of_string (List.assoc "max_ancestor_implex" conf.Config.base_env)
      with _ -> 5
    in
    Gwdb.iper_marker (Gwdb.ipers base) n
  in
  let auth = conf.Config.wizard || conf.Config.friend in
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
    if Gwdb.Marker.get mark (Gwdb.get_iper p) = 0 then ()
    else if lev = v then
      if Util.is_hide_names conf p && not (Util.authorized_age conf base p) then
        ()
      else add_surname sosa p surn dp
    else (
      Gwdb.Marker.set mark (Gwdb.get_iper p)
        (Gwdb.Marker.get mark (Gwdb.get_iper p) - 1);
      match Gwdb.get_parents p with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let fath = Util.pget conf base (Gwdb.get_father cpl) in
          let moth = Util.pget conf base (Gwdb.get_mother cpl) in
          if
            (not (Gwdb.eq_istr surn (Gwdb.get_surname fath)))
            && not (Gwdb.eq_istr surn (Gwdb.get_surname moth))
          then add_surname sosa p surn dp;
          let sosa = Sosa.twice sosa in
          (if not (Util.is_empty_person fath) then
           let dp1 = merge_date_place conf base surn dp fath in
           loop (lev + 1) sosa fath (Gwdb.get_surname fath) dp1);
          let sosa = Sosa.inc sosa 1 in
          if not (Util.is_empty_person moth) then
            let dp2 = merge_date_place conf base surn dp moth in
            loop (lev + 1) sosa moth (Gwdb.get_surname moth) dp2
      | None -> add_surname sosa p surn dp)
  in
  loop 1 Sosa.one p (Gwdb.get_surname p) (get_date_place conf base auth p);
  let list = ref [] in
  Hashtbl.iter
    (fun i dp ->
      let surn = Gwdb.sou base i in
      if surn <> "?" then list := (surn, !dp) :: !list)
    ht;
  (* TODO don't query db in sort *)
  List.sort
    (fun (s1, _) (s2, _) ->
      match
        Utf8.alphabetic_order
          (Util.surname_without_particle base s1)
          (Util.surname_without_particle base s2)
      with
      | 0 ->
          Utf8.alphabetic_order
            (Util.surname_particle base s1)
            (Util.surname_particle base s2)
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
    if not (Gwdb.is_empty_string pl) then
      let pl = Util.trimmed_string_of_place (Gwdb.sou base pl) in
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
          (pp, db, de, Gwdb.get_iper p :: l))
          p !r
  in
  (* Fonction d'ajout de tous les évènements d'une personne (birth, bapt...). *)
  let add_person p surn =
    if Gwdb.Marker.get mark (Gwdb.get_iper p) then ()
    else (
      Gwdb.Marker.set mark (Gwdb.get_iper p) true;
      add_surname p surn (Gwdb.get_birth_place p)
        (Date.od_of_cdate (Gwdb.get_birth p));
      add_surname p surn (Gwdb.get_baptism_place p)
        (Date.od_of_cdate (Gwdb.get_baptism p));
      let death = Date.date_of_death (Gwdb.get_death p) in
      add_surname p surn (Gwdb.get_death_place p) death;
      let burial =
        match Gwdb.get_burial p with
        | Def.Buried cod | Def.Cremated cod -> Date.od_of_cdate cod
        | Def.UnknownBurial -> None
      in
      add_surname p surn (Gwdb.get_burial_place p) burial;
      Array.iter
        (fun ifam ->
          let fam = Gwdb.foi base ifam in
          add_surname p surn
            (Gwdb.get_marriage_place fam)
            (Date.od_of_cdate (Gwdb.get_marriage fam)))
        (Gwdb.get_family p))
  in
  (* Parcours les ascendants de p et les ajoute dans la Hashtbl. *)
  let rec loop lev p surn =
    if lev = v then
      if Util.is_hide_names conf p && not (Util.authorized_age conf base p) then
        ()
      else add_person p surn
    else (
      add_person p surn;
      match Gwdb.get_parents p with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let fath = Util.pget conf base (Gwdb.get_father cpl) in
          let moth = Util.pget conf base (Gwdb.get_mother cpl) in
          if not (Util.is_empty_person fath) then
            loop (lev + 1) fath (Gwdb.get_surname fath);
          if not (Util.is_empty_person moth) then
            loop (lev + 1) moth (Gwdb.get_surname moth)
      | None -> ())
  in
  (* Construction de la Hashtbl. *)
  loop 1 p (Gwdb.get_surname p);
  (* On parcours la Hashtbl, et on élimine les noms vide (=?) *)
  let list = ref [] in
  Hashtbl.iter
    (fun (istr, place) ht_val ->
      let surn = Gwdb.sou base istr in
      if surn <> "?" then
        let p, db, de, pl = (fun x -> x) !ht_val in
        list := (surn, place, db, de, p, pl) :: !list)
    ht;
  (* On trie la liste par nom, puis lieu. *)
  (* TODO don't query db in sort *)
  List.sort
    (fun (s1, pl1, _, _, _, _) (s2, pl2, _, _, _, _) ->
      match
        Utf8.alphabetic_order
          (Util.surname_without_particle base s1)
          (Util.surname_without_particle base s2)
      with
      | 0 -> (
          match
            Utf8.alphabetic_order
              (Util.surname_particle base s1)
              (Util.surname_particle base s2)
          with
          | 0 ->
              Utf8.alphabetic_order
                (pl1 : Adef.escaped_string :> string)
                (pl2 : Adef.escaped_string :> string)
          | x -> x)
      | x -> x)
    !list

let linked_page_text conf base p s key (str : Adef.safe_string) (pg, (_, il)) :
    Adef.safe_string =
  match pg with
  | Def.NLDB.PgMisc pg ->
      let list = List.map snd (List.filter (fun (k, _) -> k = key) il) in
      List.fold_right
        (fun text (str : Adef.safe_string) ->
          try
            let nenv, _ = Notes.read_notes base pg in
            let v =
              let v = List.assoc s nenv in
              if v = "" then raise Not_found
              else Util.nth_field v (Util.index_of_sex (Gwdb.get_sex p))
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
                  let open Def in
                  (a : Adef.safe_string)
                  ^^^ {|<a href="|}
                  ^<^ (Util.commd conf ^^^ {|m=NOTES&f=|}
                       ^<^ (Mutil.encode pg :> Adef.escaped_string)
                       ^>^ {|#p_|}
                       ^ string_of_int text.Def.NLDB.lnPos
                        : Adef.escaped_string
                        :> Adef.safe_string)
                  ^^^ {|">|} ^<^ b ^^^ {|</a>|} ^<^ c
                in
                if (str :> string) = "" then str1
                else
                  let open Def in
                  str ^^^ ", " ^<^ str1
          with Not_found -> str)
        list str
  | Def.NLDB.PgInd _ | Def.NLDB.PgFam _ | Def.NLDB.PgNotes | Def.NLDB.PgWizard _
    ->
      str

let links_to_ind conf base db key =
  let list =
    List.fold_left
      (fun pgl (pg, (_, il)) ->
        let record_it =
          match pg with
          | Def.NLDB.PgInd ip ->
              Util.authorized_age conf base (Util.pget conf base ip)
          | Def.NLDB.PgFam ifam ->
              Util.authorized_age conf base
                (Util.pget conf base (Gwdb.get_father @@ Gwdb.foi base ifam))
          | Def.NLDB.PgNotes | Def.NLDB.PgMisc _ | Def.NLDB.PgWizard _ -> true
        in
        if record_it then
          List.fold_left
            (fun pgl (k, _) -> if k = key then pg :: pgl else pgl)
            pgl il
        else pgl)
      [] db
  in
  List.sort_uniq compare list

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
        with Failure _ -> Utf8.alphabetic_order s1 s2
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
      (string
      * Date.date option
      * Date.date option
      * string
      * Gwdb.person
      * Sosa.t list
      * TemplAst.loc)
  | Eclair of
      (string
      * Adef.safe_string
      * Date.date option
      * Date.date option
      * Gwdb.person
      * Gwdb.iper list
      * TemplAst.loc)

type title_item =
  int
  * Gwdb.istr Def.gen_title_name
  * Gwdb.istr
  * Gwdb.istr list
  * (Date.date option * Date.date option) list

type 'a env =
  | Vallgp of generation_person list
  | Vanc of generation_person
  | Vanc_surn of ancestor_surname_info
  | Vcell of cell
  | Vcelll of cell list
  | Vcnt of int ref
  | Vdesclevtab of
      ((Gwdb.iper, int) Gwdb.Marker.t * (Gwdb.ifam, int) Gwdb.Marker.t) lazy_t
  | Vdmark of (Gwdb.iper, bool) Gwdb.Marker.t ref
  | Vslist of SortedList.t ref
  | Vslistlm of string list list
  | Vind of Gwdb.person
  (* TODO Vfam should not have m_auth (last bool parameter)
     we should only have a Vfam if m_auth is true;
     this would remove many test and be safer *)
  | Vfam of Gwdb.ifam * Gwdb.family * (Gwdb.iper * Gwdb.iper * Gwdb.iper) * bool
  | Vrel of Gwdb.relation * Gwdb.person option
  | Vbool of bool
  | Vint of int
  | Vgpl of generation_person list
  | Vnldb of (Gwdb.iper, Gwdb.ifam) Def.NLDB.t
  | Vstring of string
  | Vsosa_ref of Gwdb.person option
  | Vsosa of (Gwdb.iper * (Sosa.t * Gwdb.person) option) list ref
  | Vt_sosa of SosaCache.sosa_t option
  | Vtitle of Gwdb.person * title_item
  | Vevent of Gwdb.person * Gwdb.istr Event.event_item
  | Vlazyp of string option ref
  | Vlazy of 'a env Lazy.t
  | Vother of 'a
  | Vnone

(** [has_witness_for_event event_name events] is [true] iff there is an event with name [event_name] in [events] and this event had witnesses. It do not check for permissions *)
let has_witness_for_event conf base p event_name =
  List.exists
    (fun (event_item : Gwdb.istr Event.event_item) ->
      Event.get_name event_item = event_name && Event.has_witnesses event_item)
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

let warning_use_has_parents_before_parent (fname, bp, ep) var r =
  Printf.sprintf
    "%s %d-%d: since v5.00, must test \"has_parents\" before using \"%s\"\n"
    fname bp ep var
  |> GWPARAM.syslog `LOG_WARNING;
  r

let bool_val x = TemplAst.VVbool x
let str_val x = TemplAst.VVstring x
let null_val = TemplAst.VVstring ""

let safe_val (x : [< `encoded | `escaped | `safe ] Adef.astring) =
  TemplAst.VVstring ((x :> Adef.safe_string) :> string)

let get_sosa conf base env r p =
  try List.assoc (Gwdb.get_iper p) !r
  with Not_found ->
    let s =
      match get_env "sosa_ref" env with
      | Vsosa_ref v -> (
          match get_env "t_sosa" env with
          | Vt_sosa (Some t_sosa) -> SosaCache.find_sosa conf base p v t_sosa
          | _ -> None)
      | _ -> None
    in
    r := (Gwdb.get_iper p, s) :: !r;
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
    let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
    let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
    (fn, sn, Gwdb.get_occ p)
  in
  List.fold_left (linked_page_text conf base p s key) (Adef.safe "") db

let make_ep conf base ip =
  let p = Util.pget conf base ip in
  let p_auth = Util.authorized_age conf base p in
  (p, p_auth)

(* TODO this should be a (fam * cpl) option; None if m_auth is false *)
let make_efam conf base ip ifam =
  let fam = Gwdb.foi base ifam in
  let ifath = Gwdb.get_father fam in
  let imoth = Gwdb.get_mother fam in
  let ispouse = if ip = ifath then imoth else ifath in
  let cpl = (ifath, imoth, ispouse) in
  let m_auth =
    Util.authorized_age conf base (Util.pget conf base ifath)
    && Util.authorized_age conf base (Util.pget conf base imoth)
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
    Notes.source_note_with_env conf base env (Gwdb.sou base note_source)
  else Adef.safe ""

let date_aux conf p_auth date =
  match (p_auth, Date.od_of_cdate date) with
  | true, Some d ->
      if List.assoc_opt "long_date" conf.Config.base_env = Some "yes" then
        let open Def in
        DateDisplay.string_of_ondate conf d ^>^ DateDisplay.get_wday conf d
        |> safe_val
      else DateDisplay.string_of_ondate conf d |> safe_val
  | _ -> null_val

let get_marriage_events fam =
  let fevents = Gwdb.get_fevents fam in
  List.filter (fun fe -> Gwdb.get_fevent_name fe = Def.Efam_Marriage) fevents

let get_marriage_witnesses fam =
  let marriages = get_marriage_events fam in
  let witnesses =
    List.map (fun marriage -> Gwdb.get_fevent_witnesses marriage) marriages
  in
  witnesses |> Array.concat

let get_marriage_witnesses_and_notes fam =
  let marriages = get_marriage_events fam in
  let witnesses =
    List.map
      (fun marriage -> Gwdb.get_fevent_witnesses_and_notes marriage)
      marriages
  in
  witnesses |> Array.concat

let get_nb_marriage_witnesses_of_kind fam wk =
  let witnesses = get_marriage_witnesses fam in
  Array.fold_left
    (fun acc (_, w) -> if wk = w then acc + 1 else acc)
    0 witnesses

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
    fam_check_aux (fun fam -> test @@ Gwdb.get_relation fam)
  in
  function
  | "are_divorced" ->
      fam_check_aux (fun fam ->
          match Gwdb.get_divorce fam with
          | Def.Divorced _ -> true
          | Def.NotDivorced | Def.Separated -> false)
  | "are_engaged" -> check_relation (( = ) Def.Engaged)
  | "are_married" ->
      check_relation (function
        | Def.Married | Def.NoSexesCheckMarried -> true
        | _ -> false)
  | "are_not_married" ->
      check_relation (function
        | Def.NotMarried | Def.NoSexesCheckNotMarried -> true
        | _ -> false)
  | "are_pacs" -> check_relation (( = ) Def.Pacs)
  | "are_marriage_banns" -> check_relation (( = ) Def.MarriageBann)
  | "are_marriage_contract" -> check_relation (( = ) Def.MarriageContract)
  | "are_marriage_license" -> check_relation (( = ) Def.MarriageLicense)
  | "are_residence" -> check_relation (( = ) Def.Residence)
  | "are_separated" ->
      fam_check_aux (fun fam -> Gwdb.get_divorce fam = Def.Separated)
  | "browsing_with_sosa_ref" -> (
      match get_env "sosa_ref" env with
      | Vsosa_ref v -> v <> None
      | _ -> raise Not_found)
  | "has_comment" | "has_fnotes" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          m_auth && (not conf.Config.no_note)
          && Gwdb.sou base (Gwdb.get_comment fam) <> ""
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, _, _, _) -> false
          | _ -> raise Not_found))
  | "has_fsources" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          m_auth && Gwdb.sou base (Gwdb.get_fsources fam) <> ""
      | _ -> false)
  | "has_marriage_note" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          m_auth && (not conf.Config.no_note)
          && Gwdb.sou base (Gwdb.get_marriage_note fam) <> ""
      | _ -> raise Not_found)
  | "has_marriage_source" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          m_auth && Gwdb.sou base (Gwdb.get_marriage_src fam) <> ""
      | _ -> raise Not_found)
  | "has_relation_her" -> (
      match get_env "rel" env with
      | Vrel ({ Def.r_moth = Some _ }, None) -> true
      | _ -> false)
  | "has_relation_him" -> (
      match get_env "rel" env with
      | Vrel ({ Def.r_fath = Some _ }, None) -> true
      | _ -> false)
  | "has_witnesses" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          m_auth && Array.length (Gwdb.get_witnesses fam) > 0
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
      | Vfam (_, fam, _, _) when mode_local env ->
          Gwdb.get_relation fam = Def.NoMention
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, _) -> Gwdb.get_relation fam = Def.NoMention
          | _ -> raise Not_found))
  | "is_no_sexes_check" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) when mode_local env ->
          Gwdb.get_relation fam = Def.NoSexesCheckNotMarried
          || Gwdb.get_relation fam = Def.NoSexesCheckMarried
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, _) ->
              Gwdb.get_relation fam = Def.NoSexesCheckNotMarried
              || Gwdb.get_relation fam = Def.NoSexesCheckMarried
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

and eval_simple_str_var conf base env (_, p_auth) = function
  | "alias" -> (
      match get_env "alias" env with
      | Vstring s -> s |> Util.escape_html |> safe_val
      | _ -> raise Not_found)
  | "child_cnt" -> string_of_int_env "child_cnt" env
  | "comment" | "fnotes" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          Gwdb.get_comment fam
          |> get_note_source conf base m_auth conf.Config.no_note
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
  | "divorce_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env -> (
          match Gwdb.get_divorce fam with
          | Def.Divorced d -> (
              match date_aux conf m_auth d with
              | TemplAst.VVstring s when s <> "" ->
                  TemplAst.VVstring ("<em>" ^ s ^ "</em>")
              | x -> x)
          | Def.NotDivorced | Def.Separated -> raise Not_found)
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) -> (
              match Gwdb.get_divorce fam with
              | Def.Divorced d -> (
                  match date_aux conf m_auth d with
                  | TemplAst.VVstring s when s <> "" ->
                      TemplAst.VVstring ("<em>" ^ s ^ "</em>")
                  | x -> x)
              | Def.NotDivorced | Def.Separated -> raise Not_found)
          | _ -> raise Not_found))
  | "slash_divorce_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> (
          match Gwdb.get_divorce fam with
          | Def.Divorced d -> (
              let d = Date.od_of_cdate d in
              match d with
              | Some d when m_auth ->
                  DateDisplay.string_slash_of_date conf d |> safe_val
              | _ -> null_val)
          | Def.NotDivorced | Def.Separated -> raise Not_found)
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
  | "family_cnt" -> string_of_int_env "family_cnt" env
  | "first_name_alias" -> (
      match get_env "first_name_alias" env with
      | Vstring s -> s |> Util.escape_html |> safe_val
      | _ -> null_val)
  | "fsources" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) ->
          Gwdb.get_fsources fam |> Gwdb.sou base |> Util.safe_html |> safe_val
      | _ -> null_val)
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
  | "marriage_place" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) when mode_local env ->
          if m_auth then
            Gwdb.get_marriage_place fam
            |> Gwdb.sou base |> Util.trimmed_string_of_place |> safe_val
          else null_val
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) ->
              if m_auth then
                Gwdb.get_marriage_place fam
                |> Gwdb.sou base |> Util.trimmed_string_of_place |> safe_val
              else null_val
          | _ -> raise Not_found))
  | "marriage_note" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          Gwdb.get_marriage_note fam
          |> get_note_source conf base m_auth conf.Config.no_note
      | _ -> raise Not_found)
  | "marriage_source" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) ->
          Gwdb.get_marriage_src fam |> get_note_source conf base m_auth false
      | _ -> raise Not_found)
  | "max_anc_level" -> (
      match get_env "max_anc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
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
  | "static_max_desc_level" -> (
      match get_env "static_max_desc_level" env with
      | Vint i -> str_val (string_of_int i)
      | _ -> null_val)
  | "nobility_title" -> (
      match get_env "nobility_title" env with
      | Vtitle (p, t) ->
          if p_auth then
            string_of_title conf base
              (Util.transl_nth conf "and" 0 |> Adef.safe)
              p t
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
          date_aux conf m_auth (Gwdb.get_marriage fam)
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (_, fam, _, m_auth) ->
              date_aux conf m_auth (Gwdb.get_marriage fam)
          | _ -> raise Not_found))
  | "slash_marriage_date" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> (
          match (m_auth, Date.od_of_cdate (Gwdb.get_marriage fam)) with
          | true, Some s -> DateDisplay.string_slash_of_date conf s |> safe_val
          | _ -> null_val)
      | _ -> raise Not_found)
  | "origin_file" ->
      if conf.Config.wizard then
        match get_env "fam" env with
        | Vfam (_, fam, _, _) ->
            Gwdb.get_origin_file fam |> Gwdb.sou base |> Util.escape_html
            |> safe_val
        | _ -> null_val
      else raise Not_found
  | "qualifier" -> (
      match get_env "qualifier" env with
      | Vstring nn -> nn |> Util.escape_html |> safe_val
      | _ -> raise Not_found)
  | "related_type" -> (
      match get_env "rel" env with
      | Vrel (r, Some c) ->
          Util.rchild_type_text conf r.Def.r_type (Gwdb.get_sex c) |> safe_val
      | _ -> raise Not_found)
  | "relation_type" -> (
      match get_env "rel" env with
      | Vrel (r, None) -> (
          safe_val
          @@
          match (r.Def.r_fath, r.Def.r_moth) with
          | Some ip, None | None, Some ip ->
              Util.relation_type_text conf r.Def.r_type
                (Gwdb.get_sex (Gwdb.poi base ip))
          | Some _, Some _ ->
              Util.relation_type_text conf r.Def.r_type Def.Neuter
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
  | ("baptism_witness" as s) :: sl
  | ("birth_witness" as s) :: sl
  | ("burial_witness" as s) :: sl
  | ("cremation_witness" as s) :: sl
  | ("death_witness" as s) :: sl
  | ("event_witness" as s) :: sl -> (
      match get_env s env with
      | Vind p ->
          let ep = (p, Util.authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "event_witnessed" :: sl -> (
      match get_env "event_witnessed" env with
      | Vevent (p, e) -> eval_event_witnessed_var conf base env (p, e) loc sl
      | _ -> raise Not_found)
  | "event_witness_kind" :: _ -> (
      match get_env "event_witness_kind" env with
      | Vstring wk -> TemplAst.VVstring wk
      | _ -> raise Not_found)
  | "event_witness_note" :: _ -> (
      match get_env "event_witness_note" env with
      | Vstring wnote -> TemplAst.VVstring wnote
      | _ -> raise Not_found)
  | [ "base"; "name" ] -> TemplAst.VVstring conf.Config.bname
  | [ "base"; "nb_persons" ] ->
      TemplAst.VVstring
        (Mutil.string_of_int_sep
           (Util.transl conf "(thousand separator)")
           (Gwdb.nb_of_persons base))
  | [ "base"; "real_nb_persons" ] ->
      TemplAst.VVstring
        (Mutil.string_of_int_sep
           (Util.transl conf "(thousand separator)")
           (Gwdb.nb_of_real_persons base))
  | "cell" :: sl -> (
      match get_env "cell" env with
      | Vcell cell -> eval_cell_field_var conf base env cell loc sl
      | _ -> raise Not_found)
  | "child" :: sl -> (
      match get_env "child" env with
      | Vind p when mode_local env ->
          let auth = Util.authorized_age conf base p in
          let ep = (p, auth) in
          eval_person_field_var conf base env ep loc sl
      | _ -> (
          match get_env "child_link" env with
          | Vind p ->
              let ep = (p, true) in
              let baseprefix =
                match get_env "baseprefix" env with
                | Vstring b -> b
                | _ -> conf.Config.command
              in
              let conf = { conf with command = baseprefix } in
              eval_person_field_var conf base env ep loc sl
          | _ -> raise Not_found))
  | "enclosing" :: sl ->
      let rec loop = function
        | ("#loop", _) :: env -> eval_person_field_var conf base env ep loc sl
        | _ :: env -> loop env
        | [] -> raise Not_found
      in
      loop env
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
      match Gwdb.get_parents a with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let ep = make_ep conf base (Gwdb.get_father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match
            !GWPARAM_ITL.get_father conf base conf.Config.command
              (Gwdb.get_iper a)
          with
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
      match Gwdb.get_parents a with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let ep = make_ep conf base (Gwdb.get_mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match
            !GWPARAM_ITL.get_mother conf base conf.Config.command
              (Gwdb.get_iper a)
          with
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
  | "number_of_ancestors" :: sl -> (
      match get_env "n" env with
      | Vint n -> TemplAst.VVstring (eval_num conf (Sosa.of_int (n - 1)) sl)
      | _ -> raise Not_found)
  | "number_of_descendants" :: sl -> (
      (* FIXME: what is the difference with number_of_descendants_at_level??? *)
      match get_env "level" env with
      | Vint i -> (
          match get_env "desc_level_table" env with
          | Vdesclevtab t ->
              let m = fst (Lazy.force t) in
              let cnt =
                Gwdb.Collection.fold
                  (fun cnt ip ->
                    if Gwdb.Marker.get m ip <= i then cnt + 1 else cnt)
                  0 (Gwdb.ipers base)
              in
              TemplAst.VVstring (eval_num conf (Sosa.of_int (cnt - 1)) sl)
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | "number_of_descendants_at_level" :: sl -> (
      match get_env "level" env with
      | Vint i -> (
          match get_env "desc_level_table" env with
          | Vdesclevtab t ->
              let m = fst (Lazy.force t) in
              let cnt =
                Gwdb.Collection.fold
                  (fun cnt ip ->
                    if Gwdb.Marker.get m ip <= i then cnt + 1 else cnt)
                  0 (Gwdb.ipers base)
              in
              TemplAst.VVstring (eval_num conf (Sosa.of_int (cnt - 1)) sl)
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | "parent" :: sl -> (
      match get_env "parent" env with
      | Vind p ->
          let ep = (p, Util.authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | "prev_item" :: sl -> (
      match get_env "prev_item" env with
      | Vslistlm ell -> eval_item_field_var ell sl
      | _ -> raise Not_found)
  | "prev_family" :: sl -> (
      match get_env "prev_fam" env with
      | Vfam (i, f, c, m) ->
          eval_family_field_var conf base env (i, f, c, m) loc sl
      | _ -> raise Not_found)
  | "pvar" :: v :: sl -> (
      match Util.find_person_in_env conf base v with
      | Some p ->
          let ep = make_ep conf base (Gwdb.get_iper p) in
          eval_person_field_var conf base env ep loc sl
      | None -> raise Not_found)
  | "qvar" :: v :: sl ->
      (* %qvar.index_v.surname;
         direct access to a person whose index value is v
      *)
      let v0 = Gwdb.iper_of_string v in
      (* if v0 >= 0 && v0 < nb_of_persons base then *)
      let ep = make_ep conf base v0 in
      if Util.is_empty_person (fst ep) then raise Not_found
      else eval_person_field_var conf base env ep loc sl
  (* else raise Not_found *)
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
      match Util.p_getint conf.Config.env ("s" ^ i) with
      | Some s -> (
          let s0 = Sosa.of_int s in
          let ip0 = Gwdb.get_iper p0 in
          match Util.branch_of_sosa conf base s0 (Util.pget conf base ip0) with
          | Some (p :: _) ->
              let p_auth = Util.authorized_age conf base p in
              eval_person_field_var conf base env (p, p_auth) loc sl
          | _ -> raise Not_found)
      | None -> raise Not_found)
  | "sosa_anc" :: s :: sl -> (
      (* %sosa_anc.sosa.first_name;
         direct access to a person whose sosa relative to sosa_ref is s
      *)
      match get_env "sosa_ref" env with
      | Vsosa_ref (Some p) -> (
          let ip = Gwdb.get_iper p in
          let s0 = Sosa.of_string s in
          match Util.branch_of_sosa conf base s0 (Util.pget conf base ip) with
          | Some (p :: _) ->
              let p_auth = Util.authorized_age conf base p in
              eval_person_field_var conf base env (p, p_auth) loc sl
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | "sosa_anc_p" :: s :: sl -> (
      (* %sosa_anc_p.sosa.first_name;
         direct access to a person whose sosa relative to current person
      *)
      match Util.p_of_sosa conf base (Sosa.of_string s) a with
      | Some np ->
          let np_auth = Util.authorized_age conf base np in
          eval_person_field_var conf base env (np, np_auth) loc sl
      | None -> raise Not_found)
  (* in "related" and "relation(_him/her)"
     Vrel (relation * iper option):
       relation contains the optional adoptive father/mother
       the iper option is the person which has the relation attached to (the adoptive child) *)
  | "related" :: sl -> (
      match get_env "rel" env with
      | Vrel ({ Def.r_type = rt }, Some p) ->
          eval_relation_field_var conf base env rt (Gwdb.get_iper p)
            ~is_relation:false loc sl
      | _ -> raise Not_found)
  | "relation_her" :: sl -> (
      match get_env "rel" env with
      | Vrel ({ Def.r_moth = Some ip; Def.r_type = rt }, None) ->
          eval_relation_field_var conf base env rt ip ~is_relation:true loc sl
      | _ -> raise Not_found)
  | "relation_him" :: sl -> (
      match get_env "rel" env with
      | Vrel ({ Def.r_fath = Some ip; Def.r_type = rt }, None) ->
          eval_relation_field_var conf base env rt ip ~is_relation:true loc sl
      | _ -> raise Not_found)
  | "self" :: sl -> eval_person_field_var conf base env ep loc sl
  | "sosa_ref" :: sl -> (
      match get_env "sosa_ref" env with
      | Vsosa_ref (Some p) ->
          let ep = make_ep conf base (Gwdb.get_iper p) in
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
                | _ -> conf.Config.command
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
          let ep = (p, Util.authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | sl -> eval_person_field_var conf base env ep loc sl

and eval_item_field_var ell = function
  | [ s ] -> (
      try
        match ell with
        | el :: _ ->
            let v = int_of_string s in
            let r = try List.nth el (v - 1) with Failure _ -> "" in
            TemplAst.VVstring r
        | [] -> null_val
      with Failure _ -> raise Not_found)
  | _ -> raise Not_found

(* is_relation is used to specify the direction of the relation *)
and eval_relation_field_var conf base env rt ip ~is_relation loc = function
  | [ "type" ] ->
      let sex = Gwdb.get_sex (Util.pget conf base ip) in
      if is_relation then safe_val (Util.relation_type_text conf rt sex)
      else safe_val (Util.rchild_type_text conf rt sex)
  | sl ->
      let ep = make_ep conf base ip in
      eval_person_field_var conf base env ep loc sl

and eval_cell_field_var conf base env cell loc = function
  | [ "colspan" ] -> (
      match cell with
      | Empty -> TemplAst.VVstring "1"
      | Cell (_, _, _, _, s, _) -> TemplAst.VVstring (string_of_int s))
  | "family" :: sl -> (
      match cell with
      | Cell (p, Some ifam, _, _, _, base_prefix) -> (
          if conf.Config.bname = base_prefix then
            let f, c, a = make_efam conf base (Gwdb.get_iper p) ifam in
            eval_family_field_var conf base env (ifam, f, c, a) loc sl
          else
            let conf = { conf with command = base_prefix } in
            match !GWPARAM_ITL.get_family conf base base_prefix p ifam with
            | Some (f, c, a) ->
                eval_family_field_var conf base env (ifam, f, c, a) loc sl
            | None -> assert false)
      | _ -> TemplAst.VVstring "")
  | [ "is_center" ] -> (
      match cell with
      | Cell (_, _, Center, _, _, _) -> TemplAst.VVbool true
      | _ -> TemplAst.VVbool false)
  | [ "is_empty" ] -> (
      match cell with
      | Empty -> TemplAst.VVbool true
      | _ -> TemplAst.VVbool false)
  | [ "is_left" ] -> (
      match cell with
      | Cell (_, _, Left, _, _, _) -> TemplAst.VVbool true
      | _ -> TemplAst.VVbool false)
  | [ "is_right" ] -> (
      match cell with
      | Cell (_, _, Right, _, _, _) -> TemplAst.VVbool true
      | _ -> TemplAst.VVbool false)
  | [ "is_top" ] -> (
      match cell with
      | Cell (_, _, _, false, _, _) -> TemplAst.VVbool true
      | _ -> TemplAst.VVbool false)
  | "person" :: sl -> (
      match cell with
      | Cell (p, _, _, _, _, base_prefix) ->
          if conf.Config.bname = base_prefix then
            let ep = make_ep conf base (Gwdb.get_iper p) in
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
          let f = Gwdb.foi base ifam in
          let ifath = Gwdb.get_father f in
          let imoth = Gwdb.get_mother f in
          let ispouse = if ip = ifath then imoth else ifath in
          let c = (ifath, imoth, ispouse) in
          let m_auth =
            Util.authorized_age conf base (Util.pget conf base ifath)
            && Util.authorized_age conf base (Util.pget conf base imoth)
          in
          eval_family_field_var conf base env (ifam, f, c, m_auth) loc sl
      | _ -> raise Not_found)
  | "father" :: sl -> (
      match gp with
      | GP_person (_, ip, _) -> (
          match
            (Gwdb.get_parents (Util.pget conf base ip), get_env "all_gp" env)
          with
          | Some ifam, Vallgp all_gp -> (
              let cpl = Gwdb.foi base ifam in
              match get_link all_gp (Gwdb.get_father cpl) with
              | Some gp -> eval_ancestor_field_var conf base env gp loc sl
              | None ->
                  let ep = make_ep conf base (Gwdb.get_father cpl) in
                  eval_person_field_var conf base env ep loc sl)
          | _, _ -> raise Not_found)
      | GP_same (_, _, ip) -> (
          match Gwdb.get_parents (Util.pget conf base ip) with
          | Some ifam ->
              let cpl = Gwdb.foi base ifam in
              let ep = make_ep conf base (Gwdb.get_father cpl) in
              eval_person_field_var conf base env ep loc sl
          | _ -> raise Not_found)
      | _ -> raise Not_found)
  | [ "father_sosa" ] -> (
      match (gp, get_env "all_gp" env) with
      | (GP_person (n, ip, _) | GP_same (n, _, ip)), Vallgp all_gp ->
          let n = Sosa.twice n in
          TemplAst.VVstring (parent_sosa conf base ip all_gp n Gwdb.get_father)
      | _ -> null_val)
  | [ "interval" ] -> (
      let to_string x =
        Mutil.string_of_int_sep
          (Util.transl conf "(thousand separator)")
          (int_of_string @@ Sosa.to_string x)
      in
      match gp with
      | GP_interv (Some (n1, n2, Some (n3, n4))) ->
          let n2 = Sosa.sub n2 Sosa.one in
          let n4 = Sosa.sub n4 Sosa.one in
          TemplAst.VVstring
            (to_string n1 ^ "-" ^ to_string n2 ^ " = " ^ to_string n3 ^ "-"
           ^ to_string n4)
      | GP_interv (Some (n1, n2, None)) ->
          let n2 = Sosa.sub n2 Sosa.one in
          TemplAst.VVstring (to_string n1 ^ "-" ^ to_string n2 ^ " = ...")
      | GP_interv None -> TemplAst.VVstring "..."
      | GP_person _ | GP_same _ | GP_missing _ -> null_val)
  | [ "mother_sosa" ] -> (
      match (gp, get_env "all_gp" env) with
      | (GP_person (n, ip, _) | GP_same (n, _, ip)), Vallgp all_gp ->
          let n = Sosa.inc (Sosa.twice n) 1 in
          TemplAst.VVstring (parent_sosa conf base ip all_gp n Gwdb.get_mother)
      | _ -> null_val)
  | "same" :: sl -> (
      match gp with
      | GP_same (_, n, _) -> TemplAst.VVstring (eval_num conf n sl)
      | GP_person _ | GP_interv _ | GP_missing _ -> null_val)
  | "anc_sosa" :: sl -> (
      match gp with
      | GP_person (n, _, _) | GP_same (n, _, _) ->
          TemplAst.VVstring (eval_num conf n sl)
      | GP_interv _ | GP_missing _ -> null_val)
  | "spouse" :: sl -> (
      match gp with
      | GP_person (_, ip, Some ifam) ->
          let ip = Gutil.spouse ip (Gwdb.foi base ifam) in
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
      | [ "place" ] -> safe_val (Util.trimmed_string_of_place place)
      | [ "sosa_access" ] ->
          let str, _ =
            List.fold_right
              (fun sosa (str, n) ->
                let open Def in
                ( str ^^^ "&s" ^<^ string_of_int n ^<^ "="
                  ^<^ (Sosa.to_string sosa |> Mutil.encode),
                  n + 1 ))
              sosa_list
              (Adef.encoded "", 1)
          in
          let p, _ = ep in
          let open Def in
          safe_val
            ((Util.acces_n conf base (Adef.escaped "1") p
               : Adef.escaped_string
               :> Adef.safe_string)
            ^^^ (str : Adef.encoded_string :> Adef.safe_string))
      | sl ->
          let ep = make_ep conf base (Gwdb.get_iper p) in
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
      | [ "nb_events" ] -> TemplAst.VVstring (string_of_int (List.length persl))
      | [ "nb_ind" ] ->
          Util.IperSet.elements
            (List.fold_right Util.IperSet.add persl Util.IperSet.empty)
          |> List.length |> string_of_int |> str_val
      | [ "place" ] -> safe_val place
      | sl ->
          let ep = make_ep conf base (Gwdb.get_iper p) in
          eval_person_field_var conf base env ep loc sl)

and eval_num conf n = function
  | [ "hexa" ] -> Printf.sprintf "0x%X" @@ int_of_string (Sosa.to_string n)
  | [ "octal" ] -> Printf.sprintf "0x%o" @@ int_of_string (Sosa.to_string n)
  | [ "lvl" ] -> string_of_int @@ Sosa.gen n
  | [ "v" ] -> Sosa.to_string n
  | [] -> Sosa.to_string_sep (Util.transl conf "(thousand separator)") n
  | _ -> raise Not_found

and eval_person_field_var conf base env ((p, p_auth) as ep) loc = function
  (* TODO factorize this *)
  | "baptism_date" :: sl -> (
      match Date.od_of_cdate (Gwdb.get_baptism p) with
      | Some d when p_auth -> eval_date_field_var conf d sl
      | Some _ | None -> null_val)
  | "birth_date" :: sl -> (
      match Date.od_of_cdate (Gwdb.get_birth p) with
      | Some d when p_auth -> eval_date_field_var conf d sl
      | Some _ | None -> null_val)
  | "burial_date" :: sl -> (
      match Gwdb.get_burial p with
      | Def.Buried cod when p_auth -> (
          match Date.od_of_cdate cod with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | Def.Buried _ | Def.Cremated _ | Def.UnknownBurial -> null_val)
  | "cremated_date" :: sl -> (
      match Gwdb.get_burial p with
      | Def.Cremated cod when p_auth -> (
          match Date.od_of_cdate cod with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | Def.Buried _ | Def.Cremated _ | Def.UnknownBurial -> null_val)
  | "death_date" :: sl -> (
      if not p_auth then null_val
      else
        match Date.date_of_death (Gwdb.get_death p) with
        | Some d -> eval_date_field_var conf d sl
        | None -> null_val)
  | "event" :: sl -> (
      match get_env "event" env with
      | Vevent (_, e) -> eval_event_field_var conf base env ep e loc sl
      | _ -> raise Not_found)
  | "father" :: sl -> (
      match Gwdb.get_parents p with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let ep = make_ep conf base (Gwdb.get_father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match
            !GWPARAM_ITL.get_father conf base conf.Config.command
              (Gwdb.get_iper p)
          with
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
            let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
            let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
            (fn, sn, Gwdb.get_occ p)
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
          TemplAst.VVbool r
      | _ -> raise Not_found)
  | [ "has_linked_pages" ] -> (
      match get_env "nldb" env with
      | Vnldb db ->
          let r =
            if p_auth then
              let key =
                let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
                let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
                (fn, sn, Gwdb.get_occ p)
              in
              links_to_ind conf base db key <> []
            else false
          in
          TemplAst.VVbool r
      | _ -> raise Not_found)
  | [ "has_sosa" ] -> (
      match get_env "p_link" env with
      | Vbool _ -> TemplAst.VVbool false
      | _ -> (
          match get_env "sosa" env with
          | Vsosa r -> TemplAst.VVbool (get_sosa conf base env r p <> None)
          | _ -> TemplAst.VVbool false))
  | [ "init_cache"; nb_asc; from_gen_desc; nb_desc ] -> (
      try
        let nb_asc = int_of_string nb_asc in
        let from_gen_desc = int_of_string from_gen_desc in
        let nb_desc = int_of_string nb_desc in
        let () =
          !GWPARAM_ITL.init_cache conf base (Gwdb.get_iper p) nb_asc
            from_gen_desc nb_desc
        in
        null_val
      with _ -> raise Not_found)
  | [ "linked_page"; s ] -> (
      match get_env "nldb" env with
      | Vnldb db ->
          let key =
            let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
            let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
            (fn, sn, Gwdb.get_occ p)
          in
          List.fold_left (linked_page_text conf base p s key) (Adef.safe "") db
          |> safe_val
      | _ -> raise Not_found)
  | "marriage_date" :: sl -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, true) -> (
          match Date.od_of_cdate (Gwdb.get_marriage fam) with
          | Some d -> eval_date_field_var conf d sl
          | None -> null_val)
      | _ -> raise Not_found)
  | "mother" :: sl -> (
      match Gwdb.get_parents p with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let ep = make_ep conf base (Gwdb.get_mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None -> (
          match
            !GWPARAM_ITL.get_mother conf base conf.Config.command
              (Gwdb.get_iper p)
          with
          | Some (ep, baseprefix) ->
              let conf = { conf with command = baseprefix } in
              let env = ("p_link", Vbool true) :: env in
              eval_person_field_var conf base env ep loc sl
          | None -> warning_use_has_parents_before_parent loc "mother" null_val)
      )
  | "nobility_title" :: sl -> (
      match Util.main_title conf base p with
      | Some t when p_auth ->
          let id = Gwdb.sou base t.Def.t_ident in
          let pl = Gwdb.sou base t.Def.t_place in
          eval_nobility_title_field_var (id, pl) sl
      | Some _ | None -> null_val)
  | "self" :: sl -> eval_person_field_var conf base env ep loc sl
  | "sosa" :: sl -> (
      match get_env "sosa" env with
      | Vsosa x -> (
          match get_sosa conf base env x p with
          | Some (n, _) -> TemplAst.VVstring (eval_num conf n sl)
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
                    let p = Gwdb.poi base ip in
                    let p_auth = Util.authorized_age conf base p in
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
                    let p = Gwdb.poi base ip in
                    let p_auth = Util.authorized_age conf base p in
                    eval_person_field_var conf base env (p, p_auth) loc sl)
          | None -> null_val)
      | _ -> raise Not_found)
  | "spouse" :: sl -> (
      match get_env "fam" env with
      | Vfam (ifam, _, _, _) ->
          let cpl = Gwdb.foi base ifam in
          let ip = Gutil.spouse (Gwdb.get_iper p) cpl in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found)
  | [ "var" ] -> TemplAst.VVother (eval_person_field_var conf base env ep loc)
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
          if dmy.day = 0 then null_val
          else TemplAst.VVstring (string_of_int dmy.day)
      | _ -> null_val)
  | [ "day2" ] -> (
      match d with
      | Dgreg (dmy, _) -> (
          match dmy.prec with
          | OrYear dmy2 | YearInt dmy2 ->
              if dmy2.day2 = 0 then null_val
              else TemplAst.VVstring (string_of_int dmy2.day2)
          | _ -> null_val)
      | _ -> null_val)
  | [ "julian_day" ] -> (
      match d with
      | Dgreg (dmy, _) ->
          let sdn = Date.to_sdn ~from:Dgregorian dmy in
          TemplAst.VVstring (string_of_int sdn)
      | _ -> null_val)
  | [ "month" ] -> (
      match d with
      | Dgreg (dmy, _) -> TemplAst.VVstring (DateDisplay.month_text dmy)
      | _ -> null_val)
  | [ "month2" ] -> (
      match d with
      | Dgreg (dmy, _) -> (
          match dmy.prec with
          | OrYear dmy2 | YearInt dmy2 ->
              if dmy2.month2 = 0 then null_val
              else TemplAst.VVstring (string_of_int dmy2.month2)
          | _ -> null_val)
      | _ -> null_val)
  | [ "year" ] -> (
      match d with
      | Dgreg (dmy, _) -> TemplAst.VVstring (string_of_int dmy.year)
      | _ -> null_val)
  | [ "year2" ] -> (
      match d with
      | Dgreg (dmy, _) -> (
          match dmy.prec with
          | OrYear dmy2 | YearInt dmy2 ->
              TemplAst.VVstring (string_of_int dmy2.year2)
          | _ -> null_val)
      | _ -> null_val)
  | [] ->
      DateDisplay.string_of_date_aux ~link:false conf ~sep:(Adef.safe "<br>") d
      |> safe_val
  | _ -> raise Not_found

and eval_nobility_title_field_var (id, pl) = function
  | [ "ident_key" ] -> safe_val (Mutil.encode id)
  | [ "place_key" ] -> safe_val (Mutil.encode pl)
  | [] -> TemplAst.VVstring (if pl = "" then id else id ^ " " ^ pl)
  | _ -> raise Not_found

and eval_bool_event_field base (p, p_auth) event_item = function
  | "has_date" -> p_auth && Event.get_date event_item <> Date.cdate_None
  | "has_place" -> p_auth && Gwdb.sou base (Event.get_place event_item) <> ""
  | "has_note" -> p_auth && Gwdb.sou base (Event.get_note event_item) <> ""
  | "has_src" -> p_auth && Gwdb.sou base (Event.get_src event_item) <> ""
  | "has_witnesses" -> p_auth && Event.has_witnesses event_item
  | "has_spouse" -> p_auth && Event.get_spouse_iper event_item <> None
  | "computable_age" ->
      if p_auth then
        match Date.cdate_to_dmy_opt (Gwdb.get_birth p) with
        | Some d -> not (d.day = 0 && d.month = 0 && d.prec <> Sure)
        | None -> (
            match Date.cdate_to_dmy_opt (Gwdb.get_baptism p) with
            | Some d -> not (d.day = 0 && d.month = 0 && d.prec <> Sure)
            | None -> false)
      else false
  | _ -> raise Not_found

and eval_str_event_field conf base (p, p_auth) event_item = function
  | "age" ->
      if p_auth then
        let birth_date, approx =
          match Date.cdate_to_dmy_opt (Gwdb.get_birth p) with
          | None -> (Date.cdate_to_dmy_opt (Gwdb.get_baptism p), true)
          | x -> (x, false)
        in
        match
          (birth_date, Date.cdate_to_dmy_opt (Event.get_date event_item))
        with
        | ( Some ({ prec = Sure | About | Maybe } as d1),
            Some ({ prec = Sure | About | Maybe } as d2) )
          when d1 <> d2 ->
            let a = Date.time_elapsed d1 d2 in
            let s =
              if (not approx) && d1.prec = Sure && d2.prec = Sure then ""
              else Util.transl_decline conf "possibly (date)" "" ^ " "
            in
            let open Def in
            safe_val (s ^<^ DateDisplay.string_of_age conf a)
        | _ -> null_val
      else null_val
  | "name" -> (
      if not p_auth then null_val
      else
        match Event.get_name event_item with
        | Event.Pevent name ->
            Util.string_of_pevent_name conf base name |> safe_val
        | Event.Fevent name ->
            Util.string_of_fevent_name conf base name |> safe_val)
  | "date" -> (
      if not p_auth then null_val
      else
        match Date.od_of_cdate (Event.get_date event_item) with
        | Some d -> DateDisplay.string_of_date conf d |> safe_val
        | None -> null_val)
  | "on_date" -> date_aux conf p_auth (Event.get_date event_item)
  | "place" ->
      if p_auth then
        Gwdb.sou base (Event.get_place event_item)
        |> Util.trimmed_string_of_place |> safe_val
      else null_val
  | "note" ->
      Event.get_note event_item
      |> get_note_source conf base ~p p_auth conf.Config.no_note
  | "src" ->
      Event.get_src event_item |> get_note_source conf base ~p p_auth false
  | _ -> raise Not_found

and eval_event_field_var conf base env (p, p_auth) event_item loc = function
  | "date" :: sl -> (
      match (p_auth, Date.od_of_cdate (Event.get_date event_item)) with
      | true, Some d -> eval_date_field_var conf d sl
      | _ -> null_val)
  | "spouse" :: sl -> (
      match Event.get_spouse_iper event_item with
      | Some isp ->
          let sp = Gwdb.poi base isp in
          let ep = (sp, Util.authorized_age conf base sp) in
          eval_person_field_var conf base env ep loc sl
      | None -> null_val)
  | [ s ] -> (
      try bool_val (eval_bool_event_field base (p, p_auth) event_item s)
      with Not_found ->
        eval_str_event_field conf base (p, p_auth) event_item s)
  | _ -> raise Not_found

and eval_event_witnessed_var conf base env (p, e) loc = function
  | "event" :: sl ->
      let ep = (p, Util.authorized_age conf base p) in
      eval_event_field_var conf base env ep e loc sl
  | "person" :: sl ->
      let ep = (p, Util.authorized_age conf base p) in
      eval_person_field_var conf base env ep loc sl
  | _ -> raise Not_found

and eval_bool_person_field conf base env (p, p_auth) = function
  | "access_by_key" ->
      Util.accessible_by_key conf base p (Gwdb.p_first_name base p)
        (Gwdb.p_surname base p)
  | "birthday" -> (
      match (p_auth, Date.cdate_to_dmy_opt (Gwdb.get_birth p)) with
      | true, Some d ->
          if d.prec = Sure && Gwdb.get_death p = Def.NotDead then
            d.day = conf.Config.today.day
            && d.month = conf.Config.today.month
            && d.year < conf.Config.today.year
            || (not (Date.leap_year conf.Config.today.year))
               && d.day = 29 && d.month = 2 && conf.Config.today.day = 1
               && conf.Config.today.month = 3
          else false
      | _ -> false)
  | "wedding_birthday" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, m_auth) -> (
          match (Gwdb.get_relation fam, Gwdb.get_divorce fam) with
          | (Def.Married | Def.NoSexesCheckMarried), Def.NotDivorced -> (
              match (m_auth, Date.cdate_to_dmy_opt (Gwdb.get_marriage fam)) with
              | true, Some d ->
                  let father = Util.pget conf base (Gwdb.get_father fam) in
                  let mother = Util.pget conf base (Gwdb.get_mother fam) in
                  if
                    d.prec = Sure
                    && Util.authorized_age conf base father
                    && Gwdb.get_death father = Def.NotDead
                    && Util.authorized_age conf base mother
                    && Gwdb.get_death mother = Def.NotDead
                  then
                    d.day = conf.Config.today.day
                    && d.month = conf.Config.today.month
                    && d.year < conf.Config.today.year
                    || (not (Date.leap_year conf.Config.today.year))
                       && d.day = 29 && d.month = 2 && conf.Config.today.day = 1
                       && conf.Config.today.month = 3
                  else false
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | "computable_age" ->
      if p_auth then
        match (Date.cdate_to_dmy_opt (Gwdb.get_birth p), Gwdb.get_death p) with
        | Some d, Def.NotDead -> not (d.day = 0 && d.month = 0 && d.prec <> Sure)
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
              ( Date.cdate_to_dmy_opt (Gwdb.get_birth p),
                Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) )
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
      p_auth && fst (Util.get_approx_birth_date_place base p) <> None
  | "has_approx_birth_place" ->
      p_auth && (snd (Util.get_approx_birth_date_place base p) :> string) <> ""
  | "has_approx_death_date" ->
      p_auth && fst (Util.get_approx_death_date_place base p) <> None
  | "has_approx_death_place" ->
      p_auth && (snd (Util.get_approx_death_date_place base p) :> string) <> ""
  | "has_aliases" ->
      if (not p_auth) && Util.is_hide_names conf p then false
      else Gwdb.get_aliases p <> []
  | "has_baptism_date" -> p_auth && Gwdb.get_baptism p <> Date.cdate_None
  | "has_baptism_place" ->
      p_auth && Gwdb.sou base (Gwdb.get_baptism_place p) <> ""
  | "has_baptism_source" ->
      p_auth && Gwdb.sou base (Gwdb.get_baptism_src p) <> ""
  | "has_baptism_note" ->
      p_auth && (not conf.Config.no_note)
      && Gwdb.sou base (Gwdb.get_baptism_note p) <> ""
  | "has_baptism_witnesses" ->
      p_auth
      && has_witness_for_event conf base p (Event.Pevent Def.Epers_Baptism)
  | "has_birth_date" -> p_auth && Gwdb.get_birth p <> Date.cdate_None
  | "has_birth_place" -> p_auth && Gwdb.sou base (Gwdb.get_birth_place p) <> ""
  | "has_birth_source" -> p_auth && Gwdb.sou base (Gwdb.get_birth_src p) <> ""
  | "has_birth_note" ->
      p_auth && (not conf.Config.no_note)
      && Gwdb.sou base (Gwdb.get_birth_note p) <> ""
  | "has_birth_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Def.Epers_Birth)
  | "has_burial_date" ->
      if p_auth then
        match Gwdb.get_burial p with
        | Def.Buried cod -> Date.od_of_cdate cod <> None
        | Def.Cremated _ | Def.UnknownBurial -> false
      else false
  | "has_burial_place" ->
      p_auth && Gwdb.sou base (Gwdb.get_burial_place p) <> ""
  | "has_burial_source" -> p_auth && Gwdb.sou base (Gwdb.get_burial_src p) <> ""
  | "has_burial_note" ->
      p_auth && (not conf.Config.no_note)
      && Gwdb.sou base (Gwdb.get_burial_note p) <> ""
  | "has_burial_witnesses" ->
      p_auth
      && has_witness_for_event conf base p (Event.Pevent Def.Epers_Burial)
  | "has_children" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) ->
          if Array.length (Gwdb.get_children fam) > 0 then true
          else !GWPARAM_ITL.has_children conf base p fam
      | _ -> (
          Array.exists
            (fun ifam -> [||] <> Gwdb.get_children (Gwdb.foi base ifam))
            (Gwdb.get_family p)
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
                   base conf.Config.command ifam ifath imoth
          | _ -> false))
  | "has_consanguinity" ->
      p_auth
      && Gwdb.get_consang p != Adef.fix (-1)
      && Gwdb.get_consang p >= Adef.fix_of_float 0.0001
  | "has_cremation_date" ->
      if p_auth then
        match Gwdb.get_burial p with
        | Def.Cremated cod -> Date.od_of_cdate cod <> None
        | Def.Buried _ | Def.UnknownBurial -> false
      else false
  | "has_cremation_place" ->
      p_auth && Gwdb.sou base (Gwdb.get_burial_place p) <> ""
  | "has_cremation_witnesses" ->
      p_auth
      && has_witness_for_event conf base p (Event.Pevent Def.Epers_Cremation)
  | "has_death_date" -> (
      match Gwdb.get_death p with
      | Def.Death (_, _) -> p_auth
      | Def.NotDead | Def.DeadYoung | Def.DeadDontKnowWhen | Def.DontKnowIfDead
      | Def.OfCourseDead ->
          false)
  | "has_death_place" -> p_auth && Gwdb.sou base (Gwdb.get_death_place p) <> ""
  | "has_death_source" -> p_auth && Gwdb.sou base (Gwdb.get_death_src p) <> ""
  | "has_death_note" ->
      p_auth && (not conf.Config.no_note)
      && Gwdb.sou base (Gwdb.get_death_note p) <> ""
  | "has_death_witnesses" ->
      p_auth && has_witness_for_event conf base p (Event.Pevent Def.Epers_Death)
  | "has_event" ->
      if p_auth then
        match List.assoc_opt "has_events" conf.Config.base_env with
        | Some "never" -> false
        | Some "always" ->
            Array.length (Gwdb.get_family p) > 0
            || List.length (Event.events conf base p) > 0
        | Some _ | None ->
            let events = Event.events conf base p in
            let nb_fam = Array.length (Gwdb.get_family p) in
            (* return true if there is more event information
               than basic principals events.
               we do not take in account note on event as they are shown
               on the note section.
               but we do take in account witness notes. *)
            let rec loop events nb_principal_pevents nb_marr =
              match events with
              | [] -> false
              | event_item :: events -> (
                  match Event.get_name event_item with
                  | Event.Pevent pname -> (
                      match pname with
                      | Def.Epers_Birth | Def.Epers_Baptism | Def.Epers_Death
                      | Def.Epers_Burial | Def.Epers_Cremation ->
                          if Event.has_witnesses event_item then true
                          else (
                            (match pname with
                            | Def.Epers_Birth ->
                                nb_principal_pevents.(0) <-
                                  succ nb_principal_pevents.(0)
                            | Def.Epers_Baptism ->
                                nb_principal_pevents.(1) <-
                                  succ nb_principal_pevents.(1)
                            | Def.Epers_Death ->
                                nb_principal_pevents.(2) <-
                                  succ nb_principal_pevents.(2)
                            | Def.Epers_Burial | Def.Epers_Cremation ->
                                nb_principal_pevents.(3) <-
                                  succ nb_principal_pevents.(3)
                            | _ -> ());
                            if
                              Array.exists (fun i -> i > 1) nb_principal_pevents
                            then true
                            else loop events nb_principal_pevents nb_marr)
                      | _ -> true)
                  | Fevent fname -> (
                      match fname with
                      | Def.Efam_Engage | Def.Efam_Marriage | Def.Efam_NoMention
                      | Def.Efam_NoMarriage ->
                          let nb_marr = succ nb_marr in
                          if nb_marr > nb_fam then true
                          else loop events nb_principal_pevents nb_marr
                      | Def.Efam_Divorce | Def.Efam_Separated ->
                          let place = Event.get_place event_item in
                          let note = Event.get_note event_item in
                          let src = Event.get_src event_item in
                          if
                            Gwdb.sou base place <> ""
                            || Gwdb.sou base note <> ""
                            || Gwdb.sou base src <> ""
                            || Event.has_witnesses event_item
                          then true
                          else loop events nb_principal_pevents nb_marr
                      | _ -> true))
            in
            let rec loop' = function
              | [] -> false
              | event_item :: _events when Event.has_witness_note event_item ->
                  true
              | _ :: events -> loop' events
            in
            loop events [| 0; 0; 0; 0 |] 0 || loop' events
      else false
  | "has_families" ->
      Array.length (Gwdb.get_family p) > 0
      || !GWPARAM_ITL.has_family_correspondance
           conf.Config.command (Gwdb.get_iper p)
  | "has_first_names_aliases" ->
      if (not p_auth) && Util.is_hide_names conf p then false
      else Gwdb.get_first_names_aliases p <> []
  | "has_history" -> has_history conf base p p_auth
  | "has_image" -> Image.get_portrait conf base p |> Option.is_some
  | "has_image_url" -> (
      match Image.get_portrait conf base p with
      | Some (`Url _url) -> true
      | Some (`Path _fname) -> false
      | None -> false)
  | "has_nephews_or_nieces" -> Util.has_nephews_or_nieces conf base p
  | "has_nobility_titles" -> p_auth && Util.nobtit conf base p <> []
  | "has_notes" | "has_pnotes" ->
      p_auth && (not conf.Config.no_note)
      && Gwdb.sou base (Gwdb.get_notes p) <> ""
  | "has_occupation" -> p_auth && Gwdb.sou base (Gwdb.get_occupation p) <> ""
  | "has_parents" ->
      Gwdb.get_parents p <> None
      ||
      let conf =
        match get_env "baseprefix" env with
        | Vstring baseprefix -> { conf with command = baseprefix }
        | _ -> conf
      in
      !GWPARAM_ITL.has_parents_link conf.Config.command (Gwdb.get_iper p)
  | "has_possible_duplications" -> has_possible_duplications conf base p
  | "has_psources" ->
      if Util.is_hide_names conf p && not p_auth then false
      else Gwdb.sou base (Gwdb.get_psources p) <> ""
  | "has_public_name" ->
      if (not p_auth) && Util.is_hide_names conf p then false
      else Gwdb.sou base (Gwdb.get_public_name p) <> ""
  | "has_qualifiers" ->
      if (not p_auth) && Util.is_hide_names conf p then false
      else Gwdb.get_qualifiers p <> []
  (* TODO what should this be *)
  | "has_relations" ->
      p_auth
      && (Gwdb.get_rparents p <> []
         || Relation.get_others_related conf base p <> [])
  | "has_related" -> p_auth && Relation.get_event_witnessed conf base p <> []
  | "has_siblings" -> (
      match Gwdb.get_parents p with
      | Some ifam -> Array.length (Gwdb.get_children (Gwdb.foi base ifam)) > 1
      | None ->
          let conf =
            match get_env "baseprefix" env with
            | Vstring baseprefix -> { conf with command = baseprefix }
            | _ -> conf
          in
          !GWPARAM_ITL.has_siblings conf.Config.command (Gwdb.get_iper p))
  | "has_sources" ->
      p_auth
      && (Gwdb.sou base (Gwdb.get_psources p) <> ""
         || Gwdb.sou base (Gwdb.get_birth_src p) <> ""
         || Gwdb.sou base (Gwdb.get_baptism_src p) <> ""
         || Gwdb.sou base (Gwdb.get_death_src p) <> ""
         || Gwdb.sou base (Gwdb.get_burial_src p) <> ""
         || Array.exists
              (fun ifam ->
                let fam = Gwdb.foi base ifam in
                let isp = Gutil.spouse (Gwdb.get_iper p) fam in
                let sp = Gwdb.poi base isp in
                (* On sait que p_auth vaut vrai. *)
                let m_auth = Util.authorized_age conf base sp in
                m_auth
                && (Gwdb.sou base (Gwdb.get_marriage_src fam) <> ""
                   || Gwdb.sou base (Gwdb.get_fsources fam) <> ""))
              (Gwdb.get_family p))
  | "has_surnames_aliases" ->
      if (not p_auth) && Util.is_hide_names conf p then false
      else Gwdb.get_surnames_aliases p <> []
  | "is_buried" -> (
      match Gwdb.get_burial p with
      | Def.Buried _ -> p_auth
      | Def.Cremated _ | Def.UnknownBurial -> false)
  | "is_cremated" -> (
      match Gwdb.get_burial p with
      | Def.Cremated _ -> p_auth
      | Def.Buried _ | Def.UnknownBurial -> false)
  | "is_dead" -> (
      match Gwdb.get_death p with
      | Def.Death _ | Def.DeadYoung | Def.DeadDontKnowWhen -> p_auth
      | Def.NotDead | Def.DontKnowIfDead | Def.OfCourseDead -> false)
  | "is_certainly_dead" -> (
      match Gwdb.get_death p with
      | Def.OfCourseDead -> p_auth
      (* TODOWHY : why not: | Death _ | DeadYoung -> true *)
      | Def.Death _ | Def.DeadYoung | Def.DeadDontKnowWhen | Def.NotDead
      | Def.DontKnowIfDead ->
          false)
  | "is_descendant" -> (
      match get_env "desc_mark" env with
      | Vdmark r -> Gwdb.Marker.get !r (Gwdb.get_iper p)
      | _ -> raise Not_found)
  | "is_female" -> Gwdb.get_sex p = Def.Female
  | "is_male" -> Gwdb.get_sex p = Def.Male
  | "is_invisible" ->
      not (Util.is_fully_visible_to_visitors conf base p)
      (* TODO remove is_private/public ? *)
  | "is_private" -> Gwdb.get_access p = Def.Private
  | "is_public" -> Gwdb.get_access p = Def.Public
  | "hide_private_names" -> conf.Config.hide_private_names
  | "is_restricted" ->
      (* TODO why is it not Util.is_restricted *)
      Util.is_empty_person p
  | "is_contemporary" -> GWPARAM.is_contemporary conf base p
  | _ -> raise Not_found

and eval_str_person_field conf base env ((p, p_auth) as ep) = function
  | "access" -> Util.acces conf base p |> safe_val
  | "age" -> (
      match
        (p_auth, Date.cdate_to_dmy_opt (Gwdb.get_birth p), Gwdb.get_death p)
      with
      | true, Some d, Def.NotDead ->
          Date.time_elapsed d conf.Config.today
          |> DateDisplay.string_of_age conf
          |> safe_val
      | _ -> null_val)
  | "alias" -> (
      match Gwdb.get_aliases p with
      | nn :: _ ->
          if (not p_auth) && Util.is_hide_names conf p then null_val
          else Gwdb.sou base nn |> Util.escape_html |> safe_val
      | _ -> null_val)
  | "approx_birth_place" ->
      if p_auth then Util.get_approx_birth_date_place base p |> snd |> safe_val
      else null_val
  | "approx_death_place" ->
      if p_auth then Util.get_approx_death_date_place base p |> snd |> safe_val
      else null_val
  | "auto_image_file_name" -> (
      (* TODO what do we want here? can we remove this? *)
      match Image.get_portrait_path conf base p with
      | Some (`Path s) -> str_val s
      | None -> null_val)
  | "bname_prefix" -> Util.commd conf |> safe_val
  | "birth_place" ->
      if p_auth then
        Gwdb.get_birth_place p |> Gwdb.sou base |> Util.trimmed_string_of_place
        |> safe_val
      else null_val
  | "birth_note" ->
      Gwdb.get_birth_note p
      |> get_note_source conf base ~p p_auth conf.Config.no_note
  | "birth_source" ->
      Gwdb.get_birth_src p |> get_note_source conf base ~p p_auth false
  | "baptism_place" ->
      if p_auth then
        Gwdb.get_baptism_place p |> Gwdb.sou base
        |> Util.trimmed_string_of_place |> safe_val
      else null_val
  | "baptism_note" ->
      Gwdb.get_baptism_note p
      |> get_note_source conf base ~p p_auth conf.Config.no_note
  | "baptism_source" ->
      Gwdb.get_baptism_src p |> get_note_source conf base ~p p_auth false
  | "burial_place" ->
      if p_auth then
        Gwdb.get_burial_place p |> Gwdb.sou base |> Util.trimmed_string_of_place
        |> safe_val
      else null_val
  | "burial_note" ->
      Gwdb.get_burial_note p
      |> get_note_source conf base ~p p_auth conf.Config.no_note
  | "burial_source" ->
      Gwdb.get_burial_src p |> get_note_source conf base ~p p_auth false
  | "child_length" ->
      let ifams = Gwdb.get_family p in
      let n =
        Array.fold_left
          (fun n ifam ->
            let n_fam =
              Gwdb.foi base ifam |> Gwdb.get_children |> Array.length
            in
            n + n_fam)
          0 ifams
      in
      str_val @@ string_of_int n
  | "child_name" ->
      let force_surname =
        match Gwdb.get_parents p with
        | None -> false
        | Some ifam ->
            Gwdb.foi base ifam |> Gwdb.get_father |> Util.pget conf base
            |> Gwdb.p_surname base
            |> ( <> ) (Gwdb.p_surname base p)
      in
      if force_surname then
        NameDisplay.fullname_html_of_person conf base p |> safe_val
      else NameDisplay.first_name_html_of_person conf base p |> safe_val
  | "consanguinity" ->
      if p_auth then
        Util.string_of_decimal_num conf
          (round_2_dec (Adef.float_of_fix (Gwdb.get_consang p) *. 100.0))
        ^ " %"
        |> str_val
      else null_val
  | "cremation_place" ->
      if p_auth then
        Gwdb.get_burial_place p |> Gwdb.sou base |> Util.trimmed_string_of_place
        |> safe_val
      else null_val
  | "dates" ->
      if p_auth then DateDisplay.short_dates_text conf base p |> safe_val
      else null_val
  | "death_age" ->
      if p_auth then
        match Gutil.get_birth_death_date p with
        | ( Some (Dgreg (({ prec = Sure | About | Maybe } as d1), _)),
            Some (Dgreg (({ prec = Sure | About | Maybe } as d2), _)),
            _approx )
          when d1 <> d2 ->
            DateDisplay.string_of_age conf (Date.time_elapsed d1 d2) |> safe_val
        | _ -> null_val
      else null_val
  | "death_place" ->
      if p_auth then
        Gwdb.get_death_place p |> Gwdb.sou base |> Util.trimmed_string_of_place
        |> safe_val
      else null_val
  | "death_note" ->
      Gwdb.get_death_note p
      |> get_note_source conf base ~p p_auth conf.Config.no_note
  | "death_source" ->
      Gwdb.get_death_src p |> get_note_source conf base ~p p_auth false
  | "died" -> string_of_died conf p p_auth |> safe_val
  | "father_age_at_birth" ->
      string_of_parent_age conf base ep Gwdb.get_father |> safe_val
  | "first_name" ->
      if (not p_auth) && Util.is_hide_names conf p then
        str_val (NameDisplay.hidden_name_txt :> string)
      else Gwdb.p_first_name base p |> Util.escape_html |> safe_val
  | "first_name_key" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Gwdb.p_first_name base p |> Name.lower |> Mutil.encode |> safe_val
  | "first_name_key_val" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Gwdb.p_first_name base p |> Name.lower |> str_val
  | "first_name_key_strip" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Name.strip_c (Gwdb.p_first_name base p) '"' |> str_val
  | "history_file" ->
      if not p_auth then null_val
      else
        let fn = Gwdb.sou base (Gwdb.get_first_name p) in
        let sn = Gwdb.sou base (Gwdb.get_surname p) in
        let occ = Gwdb.get_occ p in
        HistoryDiff.history_file fn sn occ |> str_val
  | "image" -> (
      match Image.get_portrait conf base p with
      | Some src -> Image.src_to_string src |> str_val
      | None -> null_val)
  | "image_html_url" -> string_of_image_url conf base ep true |> safe_val
  | "image_size" -> Image.string_of_image_size conf base ep |> str_val
  | "image_medium_size" ->
      Image.string_of_image_medium_size conf base ep |> str_val
  | "image_small_size" ->
      Image.string_of_image_small_size conf base ep |> str_val
  | "image_url" -> string_of_image_url conf base ep false |> safe_val
  | "index" -> (
      match get_env "p_link" env with
      | Vbool _ -> null_val
      | _ -> Gwdb.get_iper p |> Gwdb.string_of_iper |> Mutil.encode |> safe_val)
  | "mark_descendants" -> (
      match get_env "desc_mark" env with
      | Vdmark r ->
          let tab = Gwdb.iper_marker (Gwdb.ipers base) false in
          let rec mark_descendants len p =
            let i = Gwdb.get_iper p in
            if Gwdb.Marker.get tab i then ()
            else (
              Gwdb.Marker.set tab i true;
              let u = p in
              for i = 0 to Array.length (Gwdb.get_family u) - 1 do
                let des = Gwdb.foi base (Gwdb.get_family u).(i) in
                for i = 0 to Array.length (Gwdb.get_children des) - 1 do
                  mark_descendants (len + 1)
                    (Util.pget conf base (Gwdb.get_children des).(i))
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
              ( Date.cdate_to_dmy_opt (Gwdb.get_birth p),
                Date.cdate_to_dmy_opt (Gwdb.get_marriage fam) )
            with
            | ( Some ({ prec = Sure | About | Maybe } as d1),
                Some ({ prec = Sure | About | Maybe } as d2) ) ->
                Date.time_elapsed d1 d2
                |> DateDisplay.string_of_age conf
                |> safe_val
            | _ -> null_val
          else null_val
      | _ -> raise Not_found)
  | "mother_age_at_birth" ->
      string_of_parent_age conf base ep Gwdb.get_mother |> safe_val
  | "misc_names" ->
      if p_auth then
        let list =
          Util.nobtit conf base
          |> Gwdb.person_misc_names base p
          |> List.map Util.escape_html
        in
        let list =
          let first_name = Gwdb.p_first_name base p in
          let surname = Gwdb.p_surname base p in
          if first_name <> "?" && surname <> "?" then
            (first_name ^ " " ^ surname |> Name.lower |> Util.escape_html)
            :: list
          else list
        in
        if list <> [] then
          let open Def in
          "<ul>"
          ^<^ List.fold_left
                (fun s n -> s ^^^ "<li>" ^<^ n ^>^ "</li>")
                (Adef.safe "")
                (list : Adef.escaped_string list :> Adef.safe_string list)
          ^>^ "</ul>"
          |> safe_val
        else null_val
      else null_val
  | "nb_children_total" ->
      Array.fold_left
        (fun n ifam ->
          n + Array.length (Gwdb.get_children (Gwdb.foi base ifam)))
        0 (Gwdb.get_family p)
      |> string_of_int |> str_val
  | "nb_children" -> (
      match get_env "fam" env with
      | Vfam (_, fam, _, _) ->
          Gwdb.get_children fam |> Array.length |> string_of_int |> str_val
      | _ -> (
          match get_env "fam_link" env with
          | Vfam (ifam, _, _, _) ->
              let baseprefix =
                match get_env "baseprefix" env with
                | Vstring baseprefix -> baseprefix
                | _ -> conf.Config.command
              in
              string_of_int (!GWPARAM_ITL.nb_children baseprefix ifam)
              |> str_val
          | _ ->
              Array.fold_left
                (fun n ifam ->
                  n + Array.length (Gwdb.get_children (Gwdb.foi base ifam)))
                0 (Gwdb.get_family p)
              |> string_of_int |> str_val))
  | "nb_families" -> (
      match get_env "p_link" env with
      | Vbool _ ->
          Gwdb.get_iper p
          |> !GWPARAM_ITL.nb_families conf.Config.command
          |> string_of_int |> str_val
      | _ -> Gwdb.get_family p |> Array.length |> string_of_int |> str_val)
  | "notes" | "pnotes" ->
      Gwdb.get_notes p
      |> get_note_source conf base ~p p_auth conf.Config.no_note
  | "occ" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Gwdb.get_occ p |> string_of_int |> str_val
  | "occupation" ->
      Gwdb.get_occupation p |> get_note_source conf base ~p p_auth false
  | "on_baptism_date" -> date_aux conf p_auth (Gwdb.get_baptism p)
  | "slash_baptism_date" ->
      if p_auth then
        match Date.od_of_cdate (Gwdb.get_baptism p) with
        | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
        | None -> null_val
      else null_val
  | "on_birth_date" -> date_aux conf p_auth (Gwdb.get_birth p)
  | "slash_birth_date" ->
      if p_auth then
        match Date.od_of_cdate (Gwdb.get_birth p) with
        | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
        | None -> null_val
      else null_val
  | "slash_approx_birth_date" ->
      if p_auth then
        match fst (Util.get_approx_birth_date_place base p) with
        | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
        | None -> null_val
      else null_val
  | "on_burial_date" -> (
      match Gwdb.get_burial p with
      | Def.Buried cod -> date_aux conf p_auth cod
      | Def.Cremated _ | Def.UnknownBurial -> raise Not_found)
  | "psources" ->
      Gwdb.get_psources p |> get_note_source conf base ~p p_auth false
  | "slash_burial_date" ->
      if p_auth then
        match Gwdb.get_burial p with
        | Def.Buried cod -> (
            match Date.od_of_cdate cod with
            | Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
            | None -> null_val)
        | Def.Cremated _ | Def.UnknownBurial -> raise Not_found
      else null_val
  | "on_cremation_date" -> (
      match Gwdb.get_burial p with
      | Def.Cremated cod -> date_aux conf p_auth cod
      | Def.Buried _ | Def.UnknownBurial -> raise Not_found)
  | "slash_cremation_date" -> (
      match Gwdb.get_burial p with
      | Def.Cremated cod -> (
          match (p_auth, Date.od_of_cdate cod) with
          | true, Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
          | _ -> null_val)
      | _ -> raise Not_found)
  | "on_death_date" -> (
      match Gwdb.get_death p with
      | Def.Death (_, d) -> date_aux conf p_auth d
      | Def.NotDead | Def.DeadYoung | Def.DeadDontKnowWhen | Def.DontKnowIfDead
      | Def.OfCourseDead ->
          raise Not_found)
  | "slash_death_date" -> (
      match (p_auth, Gwdb.get_death p) with
      | true, Def.Death (_, d) ->
          Date.date_of_cdate d
          |> DateDisplay.string_slash_of_date conf
          |> safe_val
      | _ -> null_val)
  | "slash_approx_death_date" -> (
      match (p_auth, fst (Util.get_approx_death_date_place base p)) with
      | true, Some d -> DateDisplay.string_slash_of_date conf d |> safe_val
      | _ -> null_val)
  | "prev_fam_father" -> (
      match get_env "prev_fam" env with
      | Vfam (_, _, (ifath, _, _), _) ->
          Gwdb.string_of_iper ifath |> Mutil.encode |> safe_val
      | _ -> raise Not_found)
  | "prev_fam_index" -> (
      match get_env "prev_fam" env with
      | Vfam (ifam, _, _, _) ->
          Gwdb.string_of_ifam ifam |> Mutil.encode |> safe_val
      | _ -> raise Not_found)
  | "prev_fam_mother" -> (
      match get_env "prev_fam" env with
      | Vfam (_, _, (_, imoth, _), _) ->
          Gwdb.string_of_iper imoth |> Mutil.encode |> safe_val
      | _ -> raise Not_found)
  | "public_name" ->
      if (not p_auth) && Util.is_hide_names conf p then null_val
      else
        Gwdb.get_public_name p |> Gwdb.sou base |> Util.escape_html |> safe_val
  | "qualifier" -> (
      match Gwdb.get_qualifiers p with
      | nn :: _ when p_auth || not (Util.is_hide_names conf p) ->
          Gwdb.sou base nn |> Util.escape_html |> safe_val
      | _ -> null_val)
  | "sex" ->
      (* Pour éviter les traductions bizarre, on ne teste pas p_auth. *)
      Gwdb.get_sex p |> Util.index_of_sex |> string_of_int |> str_val
  | "sosa_in_list" -> (
      match get_env "all_gp" env with
      | Vallgp all_gp -> (
          match get_link all_gp (Gwdb.get_iper p) with
          | Some (GP_person (s, _, _)) -> str_val (Sosa.to_string s)
          | Some _ | None -> null_val)
      | _ -> raise Not_found)
  | "sosa_link" -> (
      match get_env "sosa" env with
      | Vsosa x -> (
          match get_sosa conf base env x p with
          | Some (n, q) ->
              Printf.sprintf "m=RL&i1=%s&i2=%s&b1=1&b2=%s"
                (Gwdb.string_of_iper (Gwdb.get_iper p))
                (Gwdb.string_of_iper (Gwdb.get_iper q))
                (Sosa.to_string n)
              |> str_val
          | None -> null_val)
      | _ -> raise Not_found)
  | "source" -> (
      match get_env "src" env with
      | Vstring s -> safe_val (Notes.source_note conf base p s)
      | _ -> raise Not_found)
  | "surname" ->
      if (not p_auth) && Util.is_hide_names conf p then
        str_val (NameDisplay.hidden_name_txt :> string)
      else Gwdb.p_surname base p |> Util.escape_html |> safe_val
  | "surname_begin" ->
      if (not p_auth) && Util.is_hide_names conf p then null_val
      else
        Gwdb.p_surname base p |> Util.surname_particle base |> Util.escape_html
        |> safe_val
  | "surname_end" ->
      if (not p_auth) && Util.is_hide_names conf p then
        str_val (NameDisplay.hidden_name_txt :> string)
      else
        Gwdb.p_surname base p
        |> Util.surname_without_particle base
        |> Util.escape_html |> safe_val
  | "surname_key" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Gwdb.p_surname base p |> Name.lower |> Mutil.encode |> safe_val
  | "surname_key_val" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Gwdb.p_surname base p |> Name.lower |> str_val
  | "surname_key_strip" ->
      if Util.is_hide_names conf p && not p_auth then null_val
      else Name.strip_c (Gwdb.p_surname base p) '"' |> str_val
  | "title" -> Util.person_title conf base p |> safe_val
  | _ -> raise Not_found

and eval_family_field_var conf base env
    ((_, fam, (ifath, imoth, _), m_auth) as fcd) loc = function
  | "father" :: sl -> (
      match get_env "f_link" env with
      | Vbool _ -> raise Not_found
      | _ ->
          let ep = make_ep conf base ifath in
          eval_person_field_var conf base env ep loc sl)
  | "marriage_date" :: sl -> (
      match Date.od_of_cdate (Gwdb.get_marriage fam) with
      | Some d when m_auth -> eval_date_field_var conf d sl
      | Some _ | None -> null_val)
  | "mother" :: sl -> (
      match get_env "f_link" env with
      | Vbool _ -> raise Not_found
      | _ ->
          let ep = make_ep conf base imoth in
          eval_person_field_var conf base env ep loc sl)
  | "marriage" :: sl -> eval_family_marriage_field_var fam sl
  | [ s ] -> str_val (eval_str_family_field env fcd s)
  | _ -> raise Not_found

and eval_family_marriage_field_var fam = function
  | [ "nb_witnesses_witness" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness |> string_of_int)
  | [ "nb_witnesses_godparent" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_GodParent
        |> string_of_int)
  | [ "nb_witnesses_civilofficer" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_CivilOfficer
        |> string_of_int)
  | [ "nb_witnesses_religiousofficer" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_ReligiousOfficer
        |> string_of_int)
  | [ "nb_witnesses_informant" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Informant
        |> string_of_int)
  | [ "nb_witnesses_attending" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Attending
        |> string_of_int)
  | [ "nb_witnesses_mentioned" ] ->
      TemplAst.VVstring
        (get_nb_marriage_witnesses_of_kind fam Def.Witness_Mentioned
        |> string_of_int)
  | [ "nb_witnesses_other" ] ->
      TemplAst.VVstring
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
  | "index" -> Gwdb.string_of_ifam ifam
  | "set_infinite_desc_level" -> (
      match get_env "desc_level_table" env with
      | Vdesclevtab levt ->
          let _, flevt = Lazy.force levt in
          Gwdb.Marker.set flevt ifam infinite;
          ""
      | _ -> raise Not_found)
  | _ -> raise Not_found

and simple_person_text conf base p p_auth : Adef.safe_string =
  match Util.main_title conf base p with
  | Some t when p_auth -> NameDisplay.title_html_of_person conf base p t
  | Some _ | None -> NameDisplay.fullname_html_of_person conf base p

and string_of_died conf p p_auth =
  Adef.safe
  @@
  if p_auth then
    let is = Util.index_of_sex (Gwdb.get_sex p) in
    match Gwdb.get_death p with
    | Def.Death (dr, _) -> (
        match dr with
        | Def.Unspecified -> Util.transl_nth conf "died" is
        | Def.Murdered -> Util.transl_nth conf "murdered" is
        | Def.Killed -> Util.transl_nth conf "killed (in action)" is
        | Def.Executed -> Util.transl_nth conf "executed (legally killed)" is
        | Def.Disappeared -> Util.transl_nth conf "disappeared" is)
    | Def.DeadYoung -> Util.transl_nth conf "died young" is
    | Def.DeadDontKnowWhen -> Util.transl_nth conf "died" is
    | Def.NotDead | Def.DontKnowIfDead | Def.OfCourseDead -> ""
  else ""

and string_of_image_url conf base (p, p_auth) html : Adef.escaped_string =
  if p_auth then
    match Image.get_portrait conf base p with
    | Some (`Path fname) ->
        let s = Unix.stat fname in
        let b = Util.acces conf base p in
        let k = Image.default_portrait_filename base p in
        Format.sprintf "%sm=IM%s&d=%d&%s&k=/%s"
          (Util.commd conf :> string)
          (if html then "H" else "")
          (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
          (b :> string)
          k
        |> Adef.escaped
    | Some (`Url url) -> Adef.escaped url (* FIXME *)
    | None -> Adef.escaped ""
  else Adef.escaped ""

and string_of_parent_age conf base (p, p_auth) parent : Adef.safe_string =
  match Gwdb.get_parents p with
  | Some ifam ->
      let cpl = Gwdb.foi base ifam in
      let pp = Util.pget conf base (parent cpl) in
      if p_auth && Util.authorized_age conf base pp then
        match
          ( Date.cdate_to_dmy_opt (Gwdb.get_birth pp),
            Date.cdate_to_dmy_opt (Gwdb.get_birth p) )
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
  | "n" | "s" | "w" | "f" | "c" ->
      let n =
        match c with
        | "n" -> (
            (* replaced by %apply;nth([...],sex) *)
            match get_env "p" env with
            | Vind p -> 1 - Util.index_of_sex (Gwdb.get_sex p)
            | _ -> 2)
        | "s" -> (
            match get_env "child" env with
            | Vind p -> Util.index_of_sex (Gwdb.get_sex p)
            | _ -> (
                match get_env "p" env with
                | Vind p -> Util.index_of_sex (Gwdb.get_sex p)
                | _ -> 2))
        | "w" -> (
            match get_env "fam" env with
            | Vfam (_, fam, _, _) ->
                if Array.length (Gwdb.get_witnesses fam) <= 1 then 0 else 1
            | _ -> 0)
        | "f" -> (
            match get_env "p" env with
            | Vind p -> if Array.length (Gwdb.get_family p) <= 1 then 0 else 1
            | _ -> 0)
        | "c" -> (
            match get_env "fam" env with
            | Vfam (_, fam, _, _) ->
                if Array.length (Gwdb.get_children fam) <= 1 then 0 else 1
            | _ -> (
                match get_env "p" env with
                | Vind p ->
                    let n =
                      Array.fold_left
                        (fun n ifam ->
                          n
                          + Array.length
                              (Gwdb.get_children (Gwdb.foi base ifam)))
                        0 (Gwdb.get_family p)
                    in
                    if n <= 1 then 0 else 1
                | _ -> 0))
        | _ -> assert false
      in
      let r = Util.translate_eval (Util.transl_nth conf s n) in
      if upp then Utf8.capitalize_fst r else r
  | _ -> Templ.eval_transl conf upp s c

let print_foreach conf base print_ast eval_expr =
  let eval_int_expr env ep e =
    let s = eval_expr env ep e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let print_foreach_alias env al ((p, p_auth) as ep) =
    if (not p_auth) && Util.is_hide_names conf p then ()
    else
      Ext_list.iter_first
        (fun first a ->
          let env = ("alias", Vstring (Gwdb.sou base a)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (Gwdb.get_aliases p)
  in
  let print_foreach_ancestor env al ep =
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
                    :: env
                  in
                  List.iter (print_ast env ep) al);
              loop false gl
        in
        loop true gpl
    | _ -> ()
  in
  let print_foreach_ancestor_level env el al ((p, _) as ep) =
    let max_level =
      match el with
      | [ [ e ] ] -> eval_int_expr env ep e
      | [] -> ( match get_env "max_anc_level" env with Vint n -> n | _ -> 0)
      | _ -> raise Not_found
    in
    let mark = Gwdb.iper_marker (Gwdb.ipers base) Sosa.zero in
    let rec loop gpl i n =
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
          ("gpl", Vgpl gpl) :: ("level", Vint i) :: ("n", Vint n) :: env
        in
        List.iter (print_ast env ep) al;
        let gpl = next_generation conf base mark gpl in
        loop gpl (succ i) n
    in
    loop [ GP_person (Sosa.one, Gwdb.get_iper p, None) ] 1 0
  in
  let print_foreach_ancestor_level2 env al ((p, _) as ep) =
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
    loop [ GP_person (Sosa.one, Gwdb.get_iper p, None) ] 1
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
    match Util.p_getenv conf.Config.env "t" with
    | Some "E" ->
        let list = build_list_eclair conf base max_level p in
        List.iter
          (fun (a, b, c, d, e, f) ->
            let b = (b : Adef.escaped_string :> Adef.safe_string) in
            let env =
              ("ancestor", Vanc_surn (Eclair (a, b, c, d, e, f, loc))) :: env
            in
            List.iter (print_ast env ep) al)
          list
    | Some "F" ->
        let list = build_surnames_list conf base max_level p in
        List.iter
          (fun (a, (((b, c, d), e), f)) ->
            let env =
              ("ancestor", Vanc_surn (Branch (a, b, c, d, e, f, loc))) :: env
            in
            List.iter (print_ast env ep) al)
          list
    | _ -> ()
  in
  let print_foreach_ancestor_tree env el al ((p, _) as ep) =
    let p, max_level =
      match el with
      | [ [ e1 ]; [ e2 ] ] ->
          let ip = Gwdb.iper_of_string @@ eval_expr env ep e1 in
          let max_level = eval_int_expr env ep e2 in
          (Util.pget conf base ip, max_level)
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
  and print_foreach_cell env al ep =
    let celll =
      match get_env "celll" env with
      | Vcelll celll -> celll
      | _ -> raise Not_found
    in
    Ext_list.iter_first
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
              | _ -> conf.Config.command
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
                (fun ip ->
                  Util.authorized_age conf base (Util.pget conf base ip))
                (Gwdb.get_children fam)
            in
            let env = ("auth", Vbool auth) :: env in
            let n =
              let p =
                match get_env "p" env with Vind p -> p | _ -> assert false
              in
              let rec loop i =
                if i = Array.length (Gwdb.get_children fam) then -2
                else if (Gwdb.get_children fam).(i) = Gwdb.get_iper p then i
                else loop (i + 1)
              in
              loop 0
            in
            Array.iteri
              (fun i ip ->
                let p = Util.pget conf base ip in
                let env = ("#loop", Vint 0) :: env in
                let env = ("child", Vind p) :: env in
                let env = ("child_cnt", Vint (i + 1)) :: env in
                let env =
                  if i = n - 1 && not (Util.is_empty_person p) then
                    ("pos", Vstring "prev") :: env
                  else if i = n then ("pos", Vstring "self") :: env
                  else if i = n + 1 && not (Util.is_empty_person p) then
                    ("pos", Vstring "next") :: env
                  else env
                in
                let ep = (p, Util.authorized_age conf base p) in
                List.iter (print_ast env ep) al)
              (Gwdb.get_children fam);

            List.iter
              (fun (_, _, children) ->
                List.iter
                  (fun ((p, _), baseprefix, can_merge) ->
                    if not can_merge then
                      let env = ("#loop", Vint 0) :: env in
                      let env = ("child_link", Vind p) :: env in
                      let env = ("baseprefix", Vstring baseprefix) :: env in
                      let env = ("p_link", Vbool true) :: env in
                      let ep = (p, true) in
                      List.iter (print_ast env ep) al)
                  children)
              (!GWPARAM_ITL.get_children' conf base
                 (Gwdb.get_iper (fst ep))
                 fam isp))
    | _ -> ()
  and print_foreach_descendant_level env al ep =
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
    Ext_list.iter_first
      (fun first evt ->
        let env = ("event", Vevent (p, evt)) :: env in
        let env = ("first", Vbool first) :: env in
        List.iter (print_ast env ep) al)
      (Event.sorted_events conf base p)
  in
  let print_foreach_epers_event_witness env al ((p, _) as ep) epers_event =
    let epers_event_witness_string =
      match epers_event with
      | Def.Epers_Burial -> "burial_witness"
      | Def.Epers_Cremation -> "cremation_witness"
      | Def.Epers_Death -> "death_witness"
      | Def.Epers_Baptism -> "batism_witness"
      | Def.Epers_Birth -> "birth_witness"
      | _ -> "" (* TODO: ? *)
    in
    List.iter
      (fun event_item ->
        if Event.get_name event_item = Event.Pevent epers_event then
          Array.iteri
            (fun i (ip, _) ->
              let p = Util.pget conf base ip in
              let env =
                (epers_event_witness_string, Vind p)
                :: ("first", Vbool (i = 0))
                :: env
              in
              List.iter (print_ast env ep) al)
            (Event.get_witnesses event_item)
        else ())
      (Event.sorted_events conf base p)
  in
  let print_foreach_event_witness env al ((_, p_auth) as ep) =
    if p_auth then
      match get_env "event" env with
      | Vevent (_, event_item) ->
          Array.iteri
            (fun i (ip, wk, wnote) ->
              let p = Util.pget conf base ip in
              let wk = Util.string_of_witness_kind conf (Gwdb.get_sex p) wk in
              let wnote = Util.escape_html (Gwdb.sou base wnote) in
              let env =
                ("event_witness", Vind p)
                :: ("event_witness_kind", Vstring (wk :> string))
                :: ( "event_witness_note",
                     Vstring (wnote : Adef.escaped_string :> string) )
                :: ("first", Vbool (i = 0))
                :: env
              in
              List.iter (print_ast env ep) al)
            (Event.get_witnesses_and_notes event_item)
      | _ -> ()
  in
  let print_foreach_event_witnessed env al ((p, p_auth) as ep) =
    (* This is the category "Presence at event" *)
    if p_auth then
      let events_witnesses = Relation.get_event_witnessed conf base p in
      List.iter
        (fun (related_person, wk, wnote, evt) ->
          let wk = Util.string_of_witness_kind conf (Gwdb.get_sex p) wk in
          let wnote = Util.escape_html wnote in
          let env = ("event_witnessed", Vevent (related_person, evt)) :: env in
          let env =
            ("event_witness_kind", Vstring (wk : Adef.safe_string :> string))
            :: ( "event_witness_note",
                 Vstring (wnote : Adef.escaped_string :> string) )
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
              if wk = witness_kind then (
                let p = Util.pget conf base ip in
                let env =
                  ("witness", Vind p) :: ("first", Vbool first) :: env
                in
                List.iter (print_ast env ep) al;
                (i + 1, false))
              else (i + 1, first))
            (0, true)
            (get_marriage_witnesses fam)
        in
        ()
    | _ -> ()
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
            let cpl = (ifath, imoth, Gwdb.get_iper spouse) in
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
        (if Array.length (Gwdb.get_family p) > 0 then
         let rec loop prev i =
           if i = Array.length (Gwdb.get_family p) then ()
           else
             let ifam = (Gwdb.get_family p).(i) in
             let fam = Gwdb.foi base ifam in
             let ifath = Gwdb.get_father fam in
             let imoth = Gwdb.get_mother fam in
             let ispouse = Gutil.spouse (Gwdb.get_iper p) fam in
             let cpl = (ifath, imoth, ispouse) in
             let m_auth =
               Util.authorized_age conf base (Util.pget conf base ifath)
               && Util.authorized_age conf base (Util.pget conf base imoth)
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
              let cpl = (ifath, imoth, Gwdb.get_iper sp) in
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
    if (not p_auth) && Util.is_hide_names conf p then ()
    else
      Ext_list.iter_first
        (fun first s ->
          let env = ("first_name_alias", Vstring (Gwdb.sou base s)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (Gwdb.get_first_names_aliases p)
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
    loop 1
  in
  let print_foreach_nobility_title env al ((p, p_auth) as ep) =
    if p_auth then
      let titles = nobility_titles_list conf base p in
      Ext_list.iter_first
        (fun first x ->
          let env = ("nobility_title", Vtitle (p, x)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        titles
  in
  let print_foreach_parent env al ((a, _) as ep) =
    match Gwdb.get_parents a with
    | Some ifam ->
        let cpl = Gwdb.foi base ifam in
        Array.iter
          (fun iper ->
            let p = Util.pget conf base iper in
            let env = ("parent", Vind p) :: env in
            List.iter (print_ast env ep) al)
          (Gwdb.get_parent_array cpl)
    | None -> ()
  in
  let print_foreach_qualifier env al ((p, p_auth) as ep) =
    if (not p_auth) && Util.is_hide_names conf p then ()
    else
      Ext_list.iter_first
        (fun first nn ->
          let env = ("qualifier", Vstring (Gwdb.sou base nn)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (Gwdb.get_qualifiers p)
  in
  let print_foreach_relation env al ((p, p_auth) as ep) =
    (* TODO print_foreach_relation and print_foreach_related
       should be merged *)
    (* This is to print relation attached to [p] (Gwdb.get_rparents) *)
    if p_auth then
      Ext_list.iter_first
        (fun first r ->
          let env = ("rel", Vrel (r, None)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (Gwdb.get_rparents p)
  in
  let print_foreach_related env al ((p, p_auth) as ep) =
    (* This is to print relation of [p], that are not attached to [p]
       but attached to persons related to [p] *)
    if p_auth then
      let l = Relation.get_others_related conf base p in
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
            (Util.transl_nth conf "person/persons" 0)
            (Gwdb.sou base (Gwdb.get_psources p))
            srcl
        in
        let srcl =
          insert
            (Util.transl_nth conf "birth" 0)
            (Gwdb.sou base (Gwdb.get_birth_src p))
            srcl
        in
        let srcl =
          insert
            (Util.transl_nth conf "baptism" 0)
            (Gwdb.sou base (Gwdb.get_baptism_src p))
            srcl
        in
        let srcl, _ =
          Array.fold_left
            (fun (srcl, i) ifam ->
              let fam = Gwdb.foi base ifam in
              let isp = Gutil.spouse (Gwdb.get_iper p) fam in
              let sp = Gwdb.poi base isp in
              (* On sait que p_auth vaut vrai. *)
              let m_auth = Util.authorized_age conf base sp in
              if m_auth then
                let lab =
                  if Array.length (Gwdb.get_family p) = 1 then ""
                  else " " ^ string_of_int i
                in
                let srcl =
                  let src_typ = Util.transl_nth conf "marriage/marriages" 0 in
                  insert (src_typ ^ lab)
                    (Gwdb.sou base (Gwdb.get_marriage_src fam))
                    srcl
                in
                let src_typ = Util.transl_nth conf "family/families" 0 in
                ( insert (src_typ ^ lab)
                    (Gwdb.sou base (Gwdb.get_fsources fam))
                    srcl,
                  i + 1 )
              else (srcl, i + 1))
            (srcl, 1) (Gwdb.get_family p)
        in
        let srcl =
          insert
            (Util.transl_nth conf "death" 0)
            (Gwdb.sou base (Gwdb.get_death_src p))
            srcl
        in
        let buri_crem_lex =
          match Gwdb.get_burial p with
          | Def.Cremated _cdate -> "cremation"
          | Def.Buried _cdate -> "burial"
          | Def.UnknownBurial ->
              "burial" (* TODOWHY what should we print here *)
        in
        insert
          (Util.transl_nth conf buri_crem_lex 0)
          (Gwdb.sou base (Gwdb.get_burial_src p))
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
    if (not p_auth) && Util.is_hide_names conf p then ()
    else
      Ext_list.iter_first
        (fun first s ->
          let env = ("surname_alias", Vstring (Gwdb.sou base s)) :: env in
          let env = ("first", Vbool first) :: env in
          List.iter (print_ast env ep) al)
        (Gwdb.get_surnames_aliases p)
  in
  let print_simple_foreach env el al ini_ep ep efam loc = function
    | "alias" -> print_foreach_alias env al ep
    | "ancestor" -> print_foreach_ancestor env al ep
    | "ancestor_level" -> print_foreach_ancestor_level env el al ep
    | "ancestor_level2" -> print_foreach_ancestor_level2 env al ep
    | "ancestor_surname" -> print_foreach_anc_surn env el al loc ep
    | "ancestor_tree_line" -> print_foreach_ancestor_tree env el al ep
    | "cell" -> print_foreach_cell env al ep
    | "child" -> print_foreach_child env al ep efam
    | "cousin_level" -> print_foreach_cousin_level env al ep
    | "descendant_level" -> print_foreach_descendant_level env al ep
    | "event" -> print_foreach_event env al ep
    | "family" -> print_foreach_family env al ini_ep ep
    | "first_name_alias" -> print_foreach_first_name_alias env al ep
    | "nobility_title" -> print_foreach_nobility_title env al ep
    | "parent" -> print_foreach_parent env al ep
    | "qualifier" -> print_foreach_qualifier env al ep
    | "related" -> print_foreach_related env al ep
    | "relation" -> print_foreach_relation env al ep
    | "sorted_list_item" -> print_foreach_sorted_list_item env al ep "list"
    | "sorted_listb_item" -> print_foreach_sorted_list_item env al ep "listb"
    | "sorted_listc_item" -> print_foreach_sorted_list_item env al ep "listc"
    | "source" -> print_foreach_source env al ep
    | "surname_alias" -> print_foreach_surname_alias env al ep
    | "witness" -> print_foreach_witness env al ep Def.Witness efam
    | "witness_godparent" ->
        print_foreach_witness env al ep Def.Witness_GodParent efam
    | "witness_civilofficer" ->
        print_foreach_witness env al ep Def.Witness_CivilOfficer efam
    | "witness_religiousofficer" ->
        print_foreach_witness env al ep Def.Witness_ReligiousOfficer efam
    | "witness_informant" ->
        print_foreach_witness env al ep Def.Witness_Informant efam
    | "witness_attending" ->
        print_foreach_witness env al ep Def.Witness_Attending efam
    | "witness_mentioned" ->
        print_foreach_witness env al ep Def.Witness_Mentioned efam
    | "witness_other" -> print_foreach_witness env al ep Def.Witness_Other efam
    | "baptism_witness" ->
        print_foreach_epers_event_witness env al ep Def.Epers_Baptism
    | "birth_witness" ->
        print_foreach_epers_event_witness env al ep Def.Epers_Birth
    | "burial_witness" ->
        print_foreach_epers_event_witness env al ep Def.Epers_Burial
    | "cremation_witness" ->
        print_foreach_epers_event_witness env al ep Def.Epers_Cremation
    | "death_witness" ->
        print_foreach_epers_event_witness env al ep Def.Epers_Death
    | "event_witness" -> print_foreach_event_witness env al ep
    | "event_witnessed" -> print_foreach_event_witnessed env al ep
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
              let auth = Util.authorized_age conf base p in
              let ep = (p, auth) in
              loop env ep efam sl
          | _ -> (
              match get_env "child_link" env with
              | Vind p ->
                  let env = ("p_link", Vbool true) :: env in
                  let env = ("f_link", Vbool true) :: env in
                  let auth = Util.authorized_age conf base p in
                  let ep = (p, auth) in
                  loop env ep efam sl
              | _ -> raise Not_found))
      | "father" :: sl -> (
          match Gwdb.get_parents a with
          | Some ifam ->
              let cpl = Gwdb.foi base ifam in
              let ((_, p_auth) as ep) =
                make_ep conf base (Gwdb.get_father cpl)
              in
              let ifath = Gwdb.get_father cpl in
              let cpl = (ifath, Gwdb.get_mother cpl, ifath) in
              let m_auth =
                p_auth
                && Util.authorized_age conf base (Util.pget conf base ifath)
              in
              let efam = Vfam (ifam, Gwdb.foi base ifam, cpl, m_auth) in
              loop env ep efam sl
          | None -> (
              let conf =
                match get_env "baseprefix" env with
                | Vstring baseprefix -> { conf with command = baseprefix }
                | _ -> conf
              in
              match !GWPARAM_ITL.get_father' conf base (Gwdb.get_iper a) with
              | Some (baseprefix, ep, ifam, fam, cpl) ->
                  let efam = Vfam (ifam, fam, cpl, true) in
                  let env = ("p_link", Vbool true) :: env in
                  let env = ("f_link", Vbool true) :: env in
                  let env = ("baseprefix", Vstring baseprefix) :: env in
                  loop env ep efam sl
              | None -> warning_use_has_parents_before_parent loc "father" ()))
      | "mother" :: sl -> (
          match Gwdb.get_parents a with
          | Some ifam ->
              let cpl = Gwdb.foi base ifam in
              let ((_, p_auth) as ep) =
                make_ep conf base (Gwdb.get_mother cpl)
              in
              let ifath = Gwdb.get_father cpl in
              let cpl = (ifath, Gwdb.get_mother cpl, ifath) in
              let m_auth =
                p_auth
                && Util.authorized_age conf base (Util.pget conf base ifath)
              in
              let efam = Vfam (ifam, Gwdb.foi base ifam, cpl, m_auth) in
              loop env ep efam sl
          | None -> (
              match !GWPARAM_ITL.get_mother' conf base (Gwdb.get_iper a) with
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
                    | _ -> conf.Config.command
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
  let vl =
    List.map (function TemplAst.VVstring s -> s | _ -> raise Not_found) vl
  in
  match (f, vl) with
  | "a_of_b", [ s1; s2 ] ->
      Util.translate_eval (Util.transl_a_of_b conf s1 s2 s2)
  | "a_of_b2", [ s1; s2; s3 ] ->
      Util.translate_eval (Util.transl_a_of_b conf s1 s2 s3)
  | "a_of_b_gr_eq_lev", [ s1; s2 ] ->
      Util.translate_eval (Util.transl_a_of_gr_eq_gen_lev conf s1 s2 s2)
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
  | "hexa", [ s ] -> Ext_string.hexa_string s
  | "initial", [ s ] ->
      if String.length s = 0 then "" else String.sub s 0 (Utf8.next s 0)
  | "lazy_print", [ v ] -> (
      match get_env "lazy_print" env with
      | Vlazyp r ->
          r := Some v;
          ""
      | _ -> raise Not_found)
  | "min", s :: sl -> (
      try
        let m =
          List.fold_right (fun s -> min (int_of_string s)) sl (int_of_string s)
        in
        string_of_int m
      with Failure _ -> raise Not_found)
  | "clean_html_tags", [ s ] ->
      (* On supprime surtout les balises qui peuvent casser la mise en page. *)
      Util.clean_html_tags s
        [ "<br */?>"; "</?p>"; "</?div>"; "</?span>"; "</?pre>" ]
  | _ -> raise Not_found

let gen_interp_templ ?(no_headers = false) menu title templ_fname conf base p =
  let ep = (p, Util.authorized_age conf base p) in
  (* TODO what is this? what are those "120" *)
  let emal =
    match Util.p_getint conf.Config.env "v" with Some i -> i | None -> 120
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
      let dlt () = make_desc_level_table conf base 120 p in
      Lazy.from_fun dlt
    in
    let desc_level_table_l_save =
      let dlt () = make_desc_level_table conf base emal p in
      Lazy.from_fun dlt
    in
    let mal () =
      Vint (Util.max_ancestor_level conf base (Gwdb.get_iper p) emal + 1)
    in
    (* Static max ancestor level *)
    let smal () =
      Vint (Util.max_ancestor_level conf base (Gwdb.get_iper p) 120 + 1)
    in
    (* Sosa_ref max ancestor level *)
    let srmal () =
      match Util.find_sosa_ref conf base with
      | Some sosa_ref ->
          Vint
            (Util.max_ancestor_level conf base (Gwdb.get_iper sosa_ref) 120 + 1)
      | None -> Vint 0
    in
    let mcl () = Vint (Cousins.max_cousin_level conf base p) in
    (* Récupère le nombre maximal de niveaux de descendance en prenant en compte les liens inter-arbres (limité à 10 générations car problématique en terme de perf). *)
    let mdl () =
      Vint
        (max
           (max_descendant_level base desc_level_table_l)
           (!GWPARAM_ITL.max_descendant_level conf base (Gwdb.get_iper p) 10))
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
      ("p_auth", Vbool (Util.authorized_age conf base p));
      ("count", Vcnt (ref 0));
      ("count1", Vcnt (ref 0));
      ("count2", Vcnt (ref 0));
      ("list", Vslist (ref SortedList.empty));
      ("listb", Vslist (ref SortedList.empty));
      ("listc", Vslist (ref SortedList.empty));
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
      match Util.open_etc_file templ_fname with
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
    if conf.Config.wizard || conf.Config.friend then None
    else
      let src =
        match Gwdb.get_parents p with
        | Some ifam -> Gwdb.sou base (Gwdb.get_origin_file (Gwdb.foi base ifam))
        | None -> ""
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.Config.base_env)
      with Not_found -> None
  in
  match passwd with
  | Some (src, passwd)
    when Util.is_that_user_and_password conf.Config.auth_scheme "" passwd
         = false ->
      Util.unauthorized conf src
  | Some _ | None -> interp_templ ?no_headers "perso" conf base p

let limit_by_tree conf =
  match
    Option.map int_of_string
      (List.assoc_opt "max_anc_tree" conf.Config.base_env)
  with
  | Some x -> max 1 x
  | None -> 7

let print_ancestors_dag conf base v p =
  let v = min (limit_by_tree conf) v in
  let set =
    let rec loop set lev ip =
      let set = Dag.Pset.add ip set in
      if lev <= 1 then set
      else
        match Gwdb.get_parents (Util.pget conf base ip) with
        | Some ifam ->
            let cpl = Gwdb.foi base ifam in
            let set = loop set (lev - 1) (Gwdb.get_mother cpl) in
            loop set (lev - 1) (Gwdb.get_father cpl)
        | None -> set
    in
    loop Dag.Pset.empty v (Gwdb.get_iper p)
  in
  let elem_txt p = DagDisplay.Item (p, Adef.safe "") in
  (* Récupère les options d'affichage. *)
  let options = Util.display_options conf in
  let vbar_txt ip =
    let p = Util.pget conf base ip in
    let open Def in
    Util.commd conf ^^^ "m=A&t=T&dag=on&v=" ^<^ string_of_int v ^<^ "&"
    ^<^ options ^^^ "&" ^<^ Util.acces conf base p
  in
  let page_title =
    Util.transl conf "tree" |> Utf8.capitalize_fst |> Adef.safe
  in
  DagDisplay.make_and_print_dag conf base elem_txt vbar_txt true set []
    page_title (Adef.escaped "")

let print_ascend conf base p =
  match
    ( Util.p_getenv conf.Config.env "t",
      Util.p_getenv conf.Config.env "dag",
      Util.p_getint conf.Config.env "v" )
  with
  | Some "T", Some "on", Some v -> print_ancestors_dag conf base v p
  | _ ->
      let templ =
        match Util.p_getenv conf.Config.env "t" with
        | Some ("E" | "F" | "H" | "L") -> "anclist"
        | Some ("D" | "G" | "M" | "N" | "P" | "X" | "Y" | "Z") -> "ancsosa"
        | Some ("A" | "C" | "T") -> "anctree"
        | _ -> "ancmenu"
      in
      interp_templ templ conf base p

let print_what_links conf base p =
  if Util.authorized_age conf base p then (
    let key =
      let fn = Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) in
      let sn = Name.lower (Gwdb.sou base (Gwdb.get_surname p)) in
      (fn, sn, Gwdb.get_occ p)
    in
    let db = Gwdb.read_nldb base in
    let db = Notes.merge_possible_aliases conf db in
    let pgl = links_to_ind conf base db key in
    let title h =
      Util.transl conf "linked pages"
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Util.transl conf ":" |> Output.print_sstring conf;
      if h then Output.print_string conf (simple_person_text conf base p true)
      else (
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (Util.commd conf);
        Output.print_string conf (Util.acces conf base p);
        Output.print_sstring conf {|">|};
        Output.print_string conf (simple_person_text conf base p true);
        Output.print_sstring conf {|</a>|})
    in
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf true;
    NotesDisplay.print_linked_list conf base pgl;
    Hutil.trailer conf)
  else Hutil.incorrect_request conf
