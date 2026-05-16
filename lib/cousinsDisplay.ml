(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Iper = Driver.Iper
module CoordMap = Cousins.CoordMap

let default_max_cnt = 2000

let give_access conf base ~cnt_sp ia_asex p1 b1 p2 b2 =
  let sps = Util.get_opt conf "sp" true in
  let img = Util.get_opt conf "im" true in
  let reference _ _ p (s : Adef.safe_string) =
    if Util.is_hidden p then s
    else
      Printf.sprintf {|<a href="%sm=RL&%s&b1=%s&%s&b2=%s%s%s&bd=%s">%s</a>|}
        (Util.commd conf :> string)
        (Util.acces_n conf base (Adef.escaped "1") p1 :> string)
        (Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b1)))
        (Util.acces_n conf base (Adef.escaped "2") p2 :> string)
        (Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b2)))
        (if sps then "" else "&sp=0")
        (if img then "" else "&im=0")
        (Option.value ~default:(Adef.encoded "0") (List.assoc_opt "bd" conf.env)
          :> string)
        (s :> string)
      |> Adef.safe
  in

  let reference_sp p3 _ _ p (s : Adef.safe_string) =
    if Util.is_hidden p then s
    else
      Printf.sprintf {|<a href="%sm=RL&%s&b1=%s&%s&b2=%s&%s%s%s&bd=%s">%s</a>|}
        (Util.commd conf :> string)
        (Util.acces_n conf base (Adef.escaped "1") p1 :> string)
        (Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b1)))
        (Util.acces_n conf base (Adef.escaped "2") p2 :> string)
        (Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b2)))
        (Util.acces_n conf base (Adef.escaped "4") p3 :> string)
        (if sps then "" else "&sp=0")
        (if img then "" else "&im=0")
        (Option.value ~default:(Adef.encoded "0") (List.assoc_opt "bd" conf.env)
          :> string)
        (s :> string)
      |> Adef.safe
  in
  let print_nospouse _ =
    SosaCache.print_sosa conf base p2 true;
    Output.print_string conf (Util.gen_person_title_text reference conf base p2);
    Output.print_string conf (DateDisplay.short_dates_text conf base p2)
  in
  let print_spouse sp first =
    incr cnt_sp;
    if first then (
      SosaCache.print_sosa conf base p2 true;
      Output.print_string conf
        (Util.gen_person_title_text reference conf base p2))
    else (
      Output.print_sstring conf "<br>";
      Output.print_string conf (Util.person_title_text conf base p2));
    Output.print_string conf (DateDisplay.short_dates_text conf base p2);
    Output.print_sstring conf " &amp; ";
    SosaCache.print_sosa conf base sp true;
    Output.print_string conf
      (Util.gen_person_title_text (reference_sp sp) conf base sp);
    Output.print_string conf (DateDisplay.short_dates_text conf base sp)
  in
  if Util.p_getenv conf.env "spouse" = Some "on" then
    match Driver.get_family p2 with
    | [||] -> print_nospouse ()
    | u ->
        Array.iteri
          (fun i ifam ->
            let cpl = Driver.foi base ifam in
            let sp =
              if Driver.get_sex p2 = Female then
                Util.pget conf base (Driver.get_father cpl)
              else Util.pget conf base (Driver.get_mother cpl)
            in
            print_spouse sp (i = 0))
          u
  else print_nospouse ()

let rec print_descend_upto conf base ~cnt ~cnt_sp max_cnt ini_p ini_br lev
    children =
  if lev > 0 && !cnt < max_cnt then (
    Output.print_sstring conf "<ul>\n";
    List.iter
      (fun (ip, ia_asex, rev_br) ->
        let p = Util.pget conf base ip in
        let get_spouse base iper ifam =
          let f = Driver.foi base ifam in
          if iper = Driver.get_father f then
            Driver.poi base (Driver.get_mother f)
          else Driver.poi base (Driver.get_father f)
        in
        let with_sp =
          if Array.length (Driver.get_family p) = 1 then
            let sp = get_spouse base ip (Driver.get_family p).(0) in
            " " ^<^ Util.transl conf "with" ^<^ " "
            ^<^ Util.person_title_text conf base sp
          else Adef.safe ""
        in
        let br = List.rev ((ip, Driver.get_sex p) :: rev_br) in
        let is_valid_rel =
          List.for_all (fun (ip, _) -> not (List.mem_assoc ip br)) ini_br
        in
        if
          is_valid_rel && !cnt < max_cnt && Cousins.has_desc_lev conf base lev p
        then (
          if lev <= 2 then (
            Output.print_sstring conf "<li>";
            if lev = 1 then (
              give_access conf base ~cnt_sp ia_asex ini_p ini_br p br;
              incr cnt)
            else
              let s : Adef.safe_string = Util.person_title_text conf base p in
              Util.transl_a_of_gr_eq_gen_lev conf
                (Util.transl_nth conf "child/children" 1)
                (s :> string)
                (s :> string)
              |> Util.translate_eval |> Utf8.capitalize_fst
              |> Output.print_sstring conf;
              Output.print_string conf with_sp;
              Output.print_sstring conf (Util.transl conf ":");
              Output.print_sstring conf
                (if (with_sp :> string) = "" then "<br>" else " "));
          Array.iter
            (fun ifam ->
              let children =
                List.map
                  (fun i ->
                    (i, ia_asex, (Driver.get_iper p, Driver.get_sex p) :: rev_br))
                  (Cousins.children_of_fam base ifam)
              in
              let sp = get_spouse base ip ifam in
              if
                Array.length (Driver.get_family p) > 1
                && lev >= 2 && children <> []
                && Cousins.has_desc_lev conf base lev sp
              then (
                Output.print_sstring conf (Util.transl conf "with");
                Output.print_sstring conf " ";
                Output.print_string conf (Util.person_title_text conf base sp);
                Output.print_sstring conf (Util.transl conf ":"));
              print_descend_upto conf base ~cnt ~cnt_sp max_cnt ini_p ini_br
                (lev - 1) children)
            (Driver.get_family p);
          if lev <= 2 then Output.print_sstring conf "</li>"))
      children;
    Output.print_sstring conf "</ul>")

let print_cousins_side_of conf base ~cnt ~cnt_sp max_cnt a ini_p ini_br lev1
    lev2 =
  let sib = Cousins.siblings conf base (Driver.get_iper a) in
  if List.exists (Cousins.sibling_has_desc_lev conf base lev2) sib then (
    if lev1 > 1 then (
      Output.print_sstring conf "<li>";
      [
        (Util.gen_person_title_text Util.no_reference conf base a
          : Adef.safe_string
          :> string);
      ]
      |> Util.cftransl conf "on %s's siblings side"
      |> Utf8.capitalize_fst |> Output.print_sstring conf;
      Output.print_sstring conf (Util.transl conf ":"));
    let sib = List.map (fun (ip, ia_asex) -> (ip, ia_asex, [])) sib in
    print_descend_upto conf base ~cnt ~cnt_sp max_cnt ini_p ini_br lev2 sib;
    if lev1 > 1 then Output.print_sstring conf "</li>";
    true)
  else false

let print_cousins_lev conf base ~cnt ~cnt_sp max_cnt p lev1 lev2 =
  let first_sosa =
    let rec loop sosa lev =
      if lev <= 1 then sosa else loop (Sosa.twice sosa) (lev - 1)
    in
    loop Sosa.one lev1
  in
  let last_sosa = Sosa.twice first_sosa in
  Perso.interp_templ "buttons_cousins" conf base p;
  Util.print_tips_relationship conf;
  if lev1 > 1 then Output.print_sstring conf "<ul>";
  let some =
    let rec loop sosa some =
      if !cnt < max_cnt && Sosa.gt last_sosa sosa then
        let some =
          match Util.old_branch_of_sosa conf base (Driver.get_iper p) sosa with
          | Some ((ia, _) :: _ as br) ->
              print_cousins_side_of conf base ~cnt ~cnt_sp max_cnt
                (Util.pget conf base ia) p br lev1 lev2
              || some
          | _ -> some
        in
        loop (Sosa.inc sosa 1) some
      else some
    in
    loop first_sosa false
  in
  if not some then (
    Output.print_sstring conf
      (Utf8.capitalize_fst (Util.transl conf "no match"));
    Output.print_sstring conf ". ");
  if lev1 > 1 then Output.print_sstring conf "</ul>"

(* HTML main *)

let print_cousins conf base p lev1 lev2 =
  let title _h =
    let cous12 = Format.sprintf "cousins.%d.%d" lev1 lev2 in
    let cous_transl = Utf8.capitalize_fst (Util.transl_nth conf cous12 1) in
    if String.length cous_transl > 0 && cous_transl.[0] <> '[' then
      Output.print_sstring conf cous_transl
    else
      Output.printf conf "%s %s / %s %s" (string_of_int lev1)
        (Util.transl_nth conf "ascending/descending (degree)"
           (if lev1 = 1 then 0 else 2))
        (string_of_int lev2)
        (Util.transl_nth conf "ascending/descending (degree)"
           (if lev2 = 1 then 1 else 3))
  in
  let max_cnt =
    try int_of_string (List.assoc "max_cousins" conf.base_env)
    with Not_found | Failure _ -> default_max_cnt
  in
  let cnt = ref 0 in
  let cnt_sp = ref 0 in
  Hutil.header conf title;
  Output.print_sstring conf "<div>";
  Output.print_sstring conf "</div>";
  let () = SosaCache.build_sosa_ht conf base in
  print_cousins_lev conf base ~cnt ~cnt_sp max_cnt p lev1 lev2;
  Output.print_sstring conf "<div><p>";
  if !cnt >= max_cnt then Output.print_sstring conf "etc. "
  else if !cnt > 1 then (
    Output.print_sstring conf (Utf8.capitalize_fst (Util.transl conf "total"));
    Output.print_sstring conf (Util.transl conf ":");
    Output.print_sstring conf " ";
    Output.print_sstring conf (string_of_int !cnt);
    Output.print_sstring conf " ";
    Output.print_sstring conf
      (Util.translate_eval ("@(c)" ^ Util.transl_nth conf "person/persons" 1)));
  if Util.p_getenv conf.env "spouse" = Some "on" then (
    Output.print_sstring conf " ";
    Output.print_sstring conf (Util.transl conf "and");
    Output.print_sstring conf " ";
    Output.print_sstring conf (string_of_int !cnt_sp);
    Output.print_sstring conf " ";
    Output.print_sstring conf
      (Util.translate_eval ("@(c)" ^ Util.transl_nth conf "spouse/spouses" 1));
    Output.print_sstring conf ". ")
  else Output.print_sstring conf ". ";
  Output.print_sstring conf "</p></div>";
  Hutil.trailer conf

(* TODO use Sosa module instead *)
let sosa_of_persons conf base =
  let rec loop n = function
    | [] -> n
    | ip :: list ->
        (* do no works if sex = Neuter *)
        loop
          (if Driver.get_sex (Util.pget conf base ip) = Male then 2 * n
           else (2 * n) + 1)
          list
  in
  loop 1

let print_anniv conf base p dead_people level =
  let s_mem x m =
    try
      let _ = Iper.Map.find x m in
      true
    with Not_found -> false
  in
  let rec insert_desc set up_sosa down_br n ip =
    if s_mem ip set then set
    else
      let set = Iper.Map.add ip (up_sosa, down_br) set in
      if n = 0 then set
      else
        let u = Driver.get_family (Util.pget conf base ip) in
        let down_br = ip :: down_br in
        let rec loop set i =
          if i = Array.length u then set
          else
            let chil = Driver.get_children (Driver.foi base u.(i)) in
            let set =
              let rec loop set i =
                if i = Array.length chil then set
                else
                  let set = insert_desc set up_sosa down_br (n - 1) chil.(i) in
                  loop set (i + 1)
              in
              loop set 0
            in
            loop set (i + 1)
        in
        loop set 0
  in
  let max_deg =
    match Util.p_getint conf.env "d" with Some d when d > 0 -> d | _ -> 0
  in
  let set =
    let module P = Pqueue.Make (struct
      type t = Driver.iper * int * int

      let leq (_, lev1, _) (_, lev2, _) = lev1 <= lev2
    end) in
    let a = P.add (Driver.get_iper p, 0, 1) P.empty in
    let max_asc = if max_deg > 0 then min level max_deg else level in
    let rec loop set a =
      if P.is_empty a then set
      else
        let (ip, n, up_sosa), a = P.take a in
        let desc_lim =
          if max_deg > 0 then min (n + 3) (max_deg - n) else n + 3
        in
        let set =
          if desc_lim >= 0 then insert_desc set up_sosa [] desc_lim ip else set
        in
        if n >= max_asc then set
        else
          let a =
            match Driver.get_parents (Util.pget conf base ip) with
            | Some ifam ->
                let cpl = Driver.foi base ifam in
                let n = n + 1 in
                let up_sosa = 2 * up_sosa in
                let a = P.add (Driver.get_father cpl, n, up_sosa) a in
                P.add (Driver.get_mother cpl, n, up_sosa + 1) a
            | None -> a
          in
          loop set a
    in
    loop Iper.Map.empty a
  in
  let set =
    Iper.Map.fold
      (fun ip (up_sosa, down_br) set ->
        let u = Driver.get_family (Util.pget conf base ip) in
        let set = Iper.Map.add ip (up_sosa, down_br, None) set in
        if Array.length u = 0 then set
        else
          let rec loop set i =
            if i = Array.length u then set
            else
              let cpl = Driver.foi base u.(i) in
              let c = Gutil.spouse ip cpl in
              loop (Iper.Map.add c (up_sosa, down_br, Some ip) set) (i + 1)
          in
          loop set 0)
      set Iper.Map.empty
  in
  let txt_of (up_sosa, down_br, spouse) conf base c =
    Printf.sprintf {|<a href="%sm=RL&%s&b1=%d&%s&b2=%d">%s</a>|}
      (Util.commd conf :> string)
      (Util.acces_n conf base (Adef.escaped "1") p :> string)
      up_sosa
      (Util.acces_n conf base (Adef.escaped "2")
         (Option.fold ~none:c ~some:(Util.pget conf base) spouse)
        :> string)
      (sosa_of_persons conf base down_br)
      (Util.person_title_text conf base c :> string)
    |> Adef.safe
  in
  let f_scan =
    let list = ref (Iper.Map.fold (fun ip b list -> (ip, b) :: list) set []) in
    fun () ->
      match !list with
      | (x, b) :: l ->
          list := l;
          (Util.pget conf base x, txt_of b)
      | [] -> raise Not_found
  in
  let mode () =
    Util.hidden_input conf "m" (Adef.encoded "C");
    Util.hidden_input conf "i"
      (Driver.get_iper p |> Driver.Iper.to_string |> Adef.encoded);
    Util.hidden_input conf "t"
      (Adef.encoded (if dead_people then "AD" else "AN"))
  in
  match Util.p_getint conf.env "v" with
  | Some i ->
      BirthdayDisplay.gen_print conf base i f_scan
        ~max_d:((2 * level) + 3)
        ~mode dead_people
  | _ ->
      if dead_people then
        BirthdayDisplay.gen_print_menu_dead conf base f_scan mode
      else BirthdayDisplay.gen_print_menu_birth conf base f_scan mode

let escape_lt_for_inline_script s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function '<' -> Buffer.add_string b "\\u003c" | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let prec_string = function
  | Adef.Sure -> ""
  | Before -> "<"
  | After -> ">"
  | About -> "~"
  | Maybe -> "?"
  | OrYear _ -> "|"
  | YearInt _ -> ".."

let json_date_opt cd =
  match Date.od_of_cdate cd with
  | Some (Dgreg (dmy, _)) when dmy.year <> 0 ->
      let int_or_null n = if n > 0 then `Int n else `Null in
      `Assoc
        [
          ("y", `Int dmy.year);
          ("m", int_or_null dmy.month);
          ("d", int_or_null dmy.day);
          ("p", `String (prec_string dmy.prec));
        ]
  | _ -> `Null

let iper_or_null ip =
  if ip = Driver.Iper.dummy then `Null else `String (Driver.Iper.to_string ip)

(* Pair raw [one_cousin] entries within a cell by their (ip, ifam chain):
   two descendants of an ancestor couple appear with identical [ifaml],
   seeded from each parent. Aggregate chain multiplicity per (ip,
   canonicalized ancestor pair) into [nbr]. A singleton (one ancestor) is
   a half-relation where the other parent of the top family is not in
   self's ancestor list. *)
let pair_paths (paths : Cousins.one_cousin list) =
  let by_chain = Hashtbl.create 64 in
  List.iter
    (fun (ip, ifaml, anc, lvl) ->
      let key = (ip, ifaml) in
      let s, _ =
        try Hashtbl.find by_chain key with Not_found -> (Iper.Set.empty, lvl)
      in
      Hashtbl.replace by_chain key (Iper.Set.add anc s, lvl))
    paths;
  let canon a b = if compare a b <= 0 then (a, Some b) else (b, Some a) in
  let by_pair = Hashtbl.create 64 in
  Hashtbl.iter
    (fun (ip, _) (anc_set, lvl) ->
      let pair =
        match Iper.Set.elements anc_set with
        | [ a ] -> (a, None)
        | [ a; b ] -> canon a b
        | _ -> (Driver.Iper.dummy, None)
      in
      let key = (ip, pair) in
      let n, l0 = try Hashtbl.find by_pair key with Not_found -> (0, lvl) in
      Hashtbl.replace by_pair key (n + 1, l0))
    by_chain;
  Hashtbl.fold
    (fun (ip, (a1, a2)) (nbr, lvl) acc -> (ip, a1, a2, nbr, lvl) :: acc)
    by_pair []

let path_to_json ~tt (ip, a1, a2, nbr, lvl) =
  `Assoc
    [
      ("ip", `String (Driver.Iper.to_string ip));
      ("a1", iper_or_null a1);
      ("a2", match a2 with None -> `Null | Some a -> iper_or_null a);
      ("nbr", `Int nbr);
      ("lvl", `Int lvl);
      ("tt", tt);
    ]

let span_to_json (smin, smax) =
  let absent = (smin = 0 && smax = 0) || (smin = 10000 && smax = -10000) in
  if absent then `Assoc [ ("min_yr", `Null); ("max_yr", `Null) ]
  else `Assoc [ ("min_yr", `Int smin); ("max_yr", `Int smax) ]

let cell_label conf i j =
  let key = Printf.sprintf "cousins.%d.%d" i j in
  let s = Util.transl_nth conf key 0 in
  let p = Util.transl_nth conf key 1 in
  let tt_key = key ^ " tt" in
  let tt_raw = Util.transl conf tt_key in
  let tt =
    if tt_raw = tt_key || tt_raw = Printf.sprintf "[%s]" tt_key then `Null
    else `String tt_raw
  in
  (s, p, tt)

let collect_persons conf base ipers =
  let meta_tbl = Hashtbl.create (Iper.Set.cardinal ipers + 1) in
  let persons_rev =
    Iper.Set.fold
      (fun ip acc ->
        let p = Driver.poi base ip in
        let alive =
          let p_auth = Util.authorized_age conf base p in
          match Driver.get_death p with
          | NotDead | DontKnowIfDead -> true
          | Death _ | DeadYoung | DeadDontKnowWhen | OfCourseDead -> not p_auth
        in
        let has_par = Driver.get_parents p <> None in
        let has_child = Array.length (Driver.get_family p) > 0 in
        let fn = Driver.sou base (Driver.get_first_name p) in
        let sn = Driver.sou base (Driver.get_surname p) in
        let oc = Driver.get_occ p in
        Hashtbl.add meta_tbl ip (alive, has_child, Driver.get_sex p, fn, sn);
        let sex =
          match Driver.get_sex p with Male -> 0 | Female -> 1 | Neuter -> 2
        in
        let vis = Util.authorized_age conf base p in
        let dates =
          if vis then (DateDisplay.short_dates_text_notag conf base p :> string)
          else ""
        in
        let birth = json_date_opt (Driver.get_birth p) in
        let death =
          match Driver.get_death p with
          | Death (_, cd) -> json_date_opt cd
          | _ -> `Null
        in
        let fn_key = Name.lower fn in
        let sn_key = Name.lower sn in
        let age_d =
          if vis then
            match Util.age_days conf base p with
            | Some n -> `Int n
            | None -> `Null
          else `Null
        in
        let json =
          `Assoc
            [
              ("fn", `String fn);
              ("sn", `String sn);
              ("fn_key", `String fn_key);
              ("sn_key", `String sn_key);
              ("oc", `Int oc);
              ("sex", `Int sex);
              ("alive", `Bool alive);
              ("has_par", `Bool has_par);
              ("has_child", `Bool has_child);
              ("vis", `Bool vis);
              ("dates", `String dates);
              ("birth", birth);
              ("death", death);
              ("age_d", age_d);
            ]
        in
        (Driver.Iper.to_string ip, json) :: acc)
      ipers []
  in
  (List.rev persons_rev, meta_tbl)

let cell_counts meta_tbl paired =
  let dist_set =
    List.fold_left
      (fun s (ip, _, _, _, _) -> Iper.Set.add ip s)
      Iper.Set.empty paired
  in
  let alive_set =
    Iper.Set.filter
      (fun ip ->
        match Hashtbl.find_opt meta_tbl ip with
        | Some (alive, _, _, _, _) -> alive
        | None -> false)
      dist_set
  in
  let no_desc_n =
    Iper.Set.fold
      (fun ip acc ->
        match Hashtbl.find_opt meta_tbl ip with
        | Some (_, has_child, _, _, _) when not has_child -> acc + 1
        | _ -> acc)
      dist_set 0
  in
  let n_paths =
    List.fold_left (fun acc (_, _, _, nbr, _) -> acc + nbr) 0 paired
  in
  (n_paths, Iper.Set.cardinal dist_set, Iper.Set.cardinal alive_set, no_desc_n)

let cell_to_json conf meta_tbl i j paths min_max =
  let paired = pair_paths paths in
  let n_paths, n_dist, n_alive, n_no_desc = cell_counts meta_tbl paired in
  let label_s, label_p, tt = cell_label conf i j in
  let name_of ip =
    match Hashtbl.find_opt meta_tbl ip with
    | Some (_, _, _, fn, sn) -> fn ^ " " ^ sn
    | None -> ""
  in
  let sex_of ip =
    match Hashtbl.find_opt meta_tbl ip with
    | Some (_, _, s, _, _) -> s
    | None -> Neuter
  in
  let path_tt (ip, a1, a2, _nbr, _lvl) =
    if j = 0 || a1 = Driver.Iper.dummy then `Null
    else
      let label =
        Util.transl_nth conf
          (Printf.sprintf "cousin.0.%d" j)
          (Util.index_of_sex (sex_of ip))
      in
      let ancs =
        match a2 with
        | None -> name_of a1
        | Some b -> name_of a1 ^ " " ^ Util.transl conf "and" ^ " " ^ name_of b
      in
      `String (Util.transl_a_of_b conf label ancs ancs)
  in
  let path_json p = path_to_json ~tt:(path_tt p) p in
  let json =
    `Assoc
      [
        ("i", `Int i);
        ("j", `Int j);
        ("label_s", `String label_s);
        ("label_p", `String label_p);
        ("tt", tt);
        ( "cnt",
          `Assoc
            [
              ("paths", `Int n_paths);
              ("dist", `Int n_dist);
              ("alive", `Int n_alive);
              ("no_desc", `Int n_no_desc);
            ] );
        ("span", span_to_json min_max);
        ("paths", `List (List.map path_json paired));
      ]
  in
  json

let cousins_level_to_json conf base (slice : Cousins.cousins_sparse) =
  let ipers =
    CoordMap.fold
      (fun _ paths acc ->
        List.fold_left
          (fun acc (ip, _, anc, _) -> Iper.Set.add ip (Iper.Set.add anc acc))
          acc paths)
      slice.data Iper.Set.empty
  in
  let persons, meta_tbl = collect_persons conf base ipers in
  let cells =
    CoordMap.fold
      (fun (i, j) paths acc ->
        if paths = [] then acc
        else
          let mm =
            try CoordMap.find (i, j) slice.dates with Not_found -> (0, 0)
          in
          cell_to_json conf meta_tbl i j paths mm :: acc)
      slice.data []
  in
  `Assoc
    [
      ("version", `Int 1);
      ("level", `Int slice.max_i);
      ("persons", `Assoc persons);
      ("cells", `List cells);
    ]

let emit_cousins_json conf base p =
  let slice0 = Cousins.init_cousins_cnt conf base ~up_to:0 p in
  let json =
    match cousins_level_to_json conf base slice0 with
    | `Assoc fields ->
        let lvl =
          match Util.p_getenv conf.env "v" with
          | Some v -> ( try int_of_string v with _ -> 0)
          | None -> 0
        in
        let abk = List.assoc_opt "access_by_key" conf.base_env = Some "yes" in
        `Assoc
          (("abk", `Bool abk)
          :: ("wizard", `Bool conf.wizard)
          :: ("lvl", `Int lvl)
          :: ("self_iper", `String (Driver.Iper.to_string (Driver.get_iper p)))
          :: fields)
    | other -> other
  in
  let json_str = escape_lt_for_inline_script (Yojson.Safe.to_string json) in
  Output.print_sstring conf
    "\n<script id=\"cousins-data\" type=\"application/json\">";
  Output.print_sstring conf json_str;
  Output.print_sstring conf "</script>\n";
  if List.assoc_opt "cousins_json_debug" conf.base_env = Some "yes" then
    try
      let oc = open_out "/tmp/cousins_debug.json" in
      output_string oc (Yojson.Safe.pretty_to_string json);
      close_out oc
    with _ -> ()

let print_cousins_json_level conf base p =
  let level =
    match Util.p_getint conf.env "json_level" with
    | Some n when n >= 0 -> n
    | _ -> 0
  in
  let sparse = Cousins.init_cousins_cnt conf base ~up_to:level p in
  let slice = Cousins.extract_level sparse level in
  let json = cousins_level_to_json conf base slice in
  let ttl_s =
    match List.assoc_opt "cache_cousins_tool" conf.base_env with
    | Some "yes" -> (
        match List.assoc_opt "cache_cousins_ttl" conf.base_env with
        | Some s -> ( try int_of_string s * 3600 with _ -> 3600)
        | None -> 3600)
    | _ -> 0
  in
  Output.header conf "Content-type: application/json; charset=UTF-8";
  Output.header conf "Cache-control: private, max-age=%d" ttl_s;
  Output.print_sstring conf (Yojson.Safe.to_string json)

let print conf base p =
  match Util.p_getint conf.env "json_level" with
  | Some _ -> print_cousins_json_level conf base p
  | _ -> (
      let max_lvl = Cousins.max_cousin_level conf in
      match
        ( Util.p_getint conf.env "v1",
          Util.p_getint conf.env "v2",
          Util.p_getenv conf.env "t" )
      with
      | Some 1, Some 1, _ | Some 0, _, _ | _, Some 0, _ ->
          Perso.interp_templ "cousins" conf base p
      | Some lvl1, _, _ ->
          let lvl1 = min (max 1 lvl1) max_lvl in
          let lvl2 =
            match Util.p_getint conf.env "v2" with
            | Some lvl2 -> min (max 1 lvl2) max_lvl
            | None -> lvl1
          in
          print_cousins conf base p lvl1 lvl2
      | _, _, Some (("AN" | "AD") as t) when conf.wizard || conf.friend ->
          print_anniv conf base p (t = "AD") max_lvl
      | _ ->
          Perso.interp_templ "cousmenu" conf base p;
          emit_cousins_json conf base p)
