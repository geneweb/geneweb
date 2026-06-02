(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Iper = Driver.Iper
module Server = Geneweb_http.Server

type 'a dag_ind = {
  di_val : 'a;
  mutable di_famc : 'a dag_fam;
  mutable di_fams : 'a dag_fam;
}

and 'a dag_fam = {
  mutable df_pare : 'a dag_ind list;
  df_chil : 'a dag_ind list;
}

let round_2_dec x = floor ((x *. 100.0) +. 0.5) /. 100.0

let dag_ind_list_of_path path =
  let indl, _ =
    let merge l1 l2 =
      if l1 == l2 then l1 else List.rev_append (List.rev l1) l2
    in
    List.fold_left
      (fun (indl, prev_ind) (ip, fl) ->
        let ind, indl =
          match List.find_opt (fun di -> di.di_val = Some ip) indl with
          | Some ind -> (ind, indl)
          | None ->
              let rec ind = { di_val = Some ip; di_famc = famc; di_fams = fams }
              and famc = { df_pare = []; df_chil = [ ind ] }
              and fams = { df_pare = [ ind ]; df_chil = [] } in
              (ind, ind :: indl)
        in
        let fam =
          match prev_ind with
          | None -> { df_pare = []; df_chil = [] }
          | Some p_ind -> (
              match fl with
              | Relation.Parent ->
                  {
                    df_pare = merge p_ind.di_famc.df_pare ind.di_fams.df_pare;
                    df_chil = merge p_ind.di_famc.df_chil ind.di_fams.df_chil;
                  }
              | Relation.Child ->
                  {
                    df_pare = merge p_ind.di_fams.df_pare ind.di_famc.df_pare;
                    df_chil = merge p_ind.di_fams.df_chil ind.di_famc.df_chil;
                  }
              | Relation.Sibling | Relation.HalfSibling ->
                  {
                    df_pare = merge p_ind.di_famc.df_pare ind.di_famc.df_pare;
                    df_chil = merge p_ind.di_famc.df_chil ind.di_famc.df_chil;
                  }
              | Relation.Mate ->
                  {
                    df_pare = merge p_ind.di_fams.df_pare ind.di_fams.df_pare;
                    df_chil = merge p_ind.di_fams.df_chil ind.di_fams.df_chil;
                  }
              | Relation.Self -> { df_pare = []; df_chil = [] })
        in
        List.iter (fun ind -> ind.di_famc <- fam) fam.df_chil;
        List.iter (fun ind -> ind.di_fams <- fam) fam.df_pare;
        (indl, Some ind))
      ([], None) (List.rev path)
  in
  indl

let add_missing_parents_of_siblings conf base indl =
  List.fold_right
    (fun ind indl ->
      let indl =
        match ind.di_famc with
        | { df_pare = []; df_chil = [ _ ] } -> indl
        | { df_pare = []; df_chil = children } ->
            let ipl =
              let _, ipl =
                List.fold_right
                  (fun ind (seen, ipl) ->
                    match ind.di_val with
                    | Some ip ->
                        let ip =
                          match Driver.get_parents (Util.pget conf base ip) with
                          | Some ifam ->
                              Driver.get_father (Driver.foi base ifam)
                          | None ->
                              failwith
                                "add_missing_parents_of_siblings: child \
                                 without parents"
                        in
                        if Iper.Set.mem ip seen then (seen, ipl)
                        else (Iper.Set.add ip seen, ip :: ipl)
                    | None ->
                        failwith
                          "add_missing_parents_of_siblings: vertex without iper")
                  children (Iper.Set.empty, [])
              in
              ipl
            in
            let fams = { df_pare = []; df_chil = children } in
            let indl1 =
              List.fold_left
                (fun indl ip ->
                  let rec indp =
                    { di_val = Some ip; di_famc = famc; di_fams = fams }
                  and famc = { df_pare = []; df_chil = [ indp ] } in
                  fams.df_pare <- indp :: fams.df_pare;
                  indp :: indl)
                [] ipl
            in
            List.iter (fun ind -> ind.di_famc <- fams) children;
            List.rev_append (List.rev indl1) indl
        | _ -> indl
      in
      ind :: indl)
    indl []

let dag_fam_list_of_ind_list indl =
  List.fold_left
    (fun faml ind ->
      let faml =
        if List.memq ind.di_famc faml then faml else ind.di_famc :: faml
      in
      if List.memq ind.di_fams faml then faml else ind.di_fams :: faml)
    [] indl

let add_phony_children indl faml =
  List.fold_right
    (fun fam indl ->
      match fam with
      | { df_pare = [ _ ]; df_chil = [] } -> indl
      | { df_pare = pare; df_chil = [] } ->
          let rec ind = { di_val = None; di_famc = famc; di_fams = fams }
          and famc = { df_pare = pare; df_chil = [ ind ] }
          and fams = { df_pare = [ ind ]; df_chil = [] } in
          List.iter (fun ind -> ind.di_fams <- famc) pare;
          ind :: indl
      | _ -> indl)
    faml indl

let add_common_parent base ip1 ip2 set =
  let a1 = Driver.poi base ip1 in
  let a2 = Driver.poi base ip2 in
  match (Driver.get_parents a1, Driver.get_parents a2) with
  | Some ifam1, Some ifam2 ->
      let cpl1 = Driver.foi base ifam1 in
      let cpl2 = Driver.foi base ifam2 in
      if Driver.get_father cpl1 = Driver.get_father cpl2 then
        Iper.Set.add (Driver.get_father cpl1) set
      else if Driver.get_mother cpl1 = Driver.get_mother cpl2 then
        Iper.Set.add (Driver.get_mother cpl1) set
      else set
  | _ -> set

let ind_set_of_relation_path base path =
  let set, _ =
    List.fold_left
      (fun (set, prev_ip) (ip, fl) ->
        let set =
          match fl with
          | Relation.Parent | Relation.Child | Relation.Self | Relation.Mate ->
              set
          | Relation.Sibling | Relation.HalfSibling -> (
              match prev_ip with
              | Some prev_ip -> add_common_parent base prev_ip ip set
              | None -> set)
        in
        (Iper.Set.add ip set, Some ip))
      (Iper.Set.empty, None) (List.rev path)
  in
  Iper.Set.elements set

let excl_faml conf base =
  let rec loop list i =
    match Util.p_getenv conf.Config.env ("ef" ^ string_of_int i) with
    | Some k -> loop (Driver.Ifam.of_string k :: list) (i + 1)
    | None -> (
        match Util.find_person_in_env conf base ("ef" ^ string_of_int i) with
        | Some p ->
            let n =
              Util.p_getint conf.env ("fef" ^ string_of_int i)
              |> Option.value ~default:0
            in
            let list =
              if n < Array.length (Driver.get_family p) then
                (Driver.get_family p).(n) :: list
              else list
            in
            loop list (i + 1)
        | None -> list)
  in
  loop [] 0

let dag_of_ind_dag_list indl =
  let indl, _ =
    List.fold_right
      (fun ind (indl, cnt) -> ((ind, cnt) :: indl, cnt + 1))
      indl ([], 0)
  in
  let idag_of_di_ind ind = Dag2html.idag_of_int (List.assq ind indl) in
  List.map
    (fun (ind, cnt) ->
      {
        Dag2html.pare = List.map idag_of_di_ind ind.di_famc.df_pare;
        valu =
          (match ind.di_val with
          | Some ic -> Def.Left ic
          | None -> Def.Right cnt);
        chil = List.map idag_of_di_ind ind.di_fams.df_chil;
      })
    indl

let dag_of_relation_path conf base path =
  let indl = dag_ind_list_of_path path in
  let indl = add_missing_parents_of_siblings conf base indl in
  let faml = dag_fam_list_of_ind_list indl in
  let indl = add_phony_children indl faml in
  let nl = dag_of_ind_dag_list indl in
  let d = { Dag2html.dag = Array.of_list (List.rev nl) } in
  let set =
    List.fold_left
      (fun set n ->
        match n.Dag2html.valu with
        | Def.Left ip -> Iper.Set.add ip set
        | Def.Right _ -> set)
      Iper.Set.empty nl
    |> Iper.Set.elements
  in
  (set, d)

let old_print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  let invert = Util.p_getenv conf.env "invert" = Some "on" in
  let set, d = dag_of_relation_path conf base path in
  let page_title =
    Util.transl conf "relationship" |> Utf8.capitalize_fst |> Adef.safe
  in
  let hts =
    DagDisplay.make_tree_hts conf base elem_txt vbar_txt invert set [] d
  in
  DagDisplay.print_dag_page conf base page_title hts next_txt

let print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  if Util.p_getenv conf.env "new" <> Some "on" then
    old_print_relationship_dag conf base elem_txt vbar_txt path next_txt
  else
    let invert = Util.p_getenv conf.env "invert" = Some "on" in
    let set = ind_set_of_relation_path base path in
    let page_title =
      Util.transl conf "relationship" |> Utf8.capitalize_fst |> Adef.safe
    in
    DagDisplay.make_and_print_dag conf base elem_txt vbar_txt invert set []
      page_title next_txt

let next_relation_link_txt conf ip1 ip2 excl_faml : Adef.escaped_string =
  let sps =
    match (Util.p_getenv conf.env "sp", Util.p_getenv conf.env "spouse") with
    | Some ("off" | "0"), _ | _, Some "off" -> false
    | _, _ -> true
  in
  let sl, _ =
    List.fold_left
      (fun (sl, i) ifam ->
        ("&ef" ^ string_of_int i ^ "=" ^ Driver.Ifam.to_string ifam ^ sl, i - 1))
      ("", List.length excl_faml - 1)
      excl_faml
  in
  Util.commd ~excl:[ "em"; "ei"; "i"; "sp"; "et" ] conf
  ^^^ "em=R&ei=" ^<^ Driver.Iper.to_string ip1 ^<^ "&i="
  ^<^ Driver.Iper.to_string ip2
  ^<^ (if sps then "" else "&sp=0")
  ^<^ Adef.escaped ("&et=S" ^ sl)

let print_relation_path conf base ip1 ip2 path ifam excl_faml =
  if path = [] then (
    let title _ =
      Util.transl conf "relationship"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Hutil.trailer conf)
  else
    let next_txt = next_relation_link_txt conf ip1 ip2 (ifam :: excl_faml) in
    let elem_txt p = DagDisplay.Item (p, Adef.safe "") in
    let vbar_txt ip =
      let u = Util.pget conf base ip in
      let excl_faml =
        Array.fold_right List.cons (Driver.get_family u) excl_faml
      in
      next_relation_link_txt conf ip1 ip2 excl_faml
    in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print_shortest_path conf base p1 p2 =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  if ip1 = ip2 then (
    let title _ =
      Util.transl conf "relationship"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Util.transl conf "it is the same person!"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf "\n";
    Hutil.trailer conf)
  else
    let excl_faml = excl_faml conf base in
    let title _ =
      Util.transl conf "relationship"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    match Relation.get_shortest_path_relation conf base ip1 ip2 excl_faml with
    | Some (path, ifam) ->
        print_relation_path conf base ip1 ip2 path ifam excl_faml
    | None ->
        let s1 = Util.gen_person_title_text Util.reference conf base p1 in
        let s2 = Util.gen_person_title_text Util.reference conf base p2 in
        Hutil.header conf title;
        (match Util.p_getenv conf.env "cgl" with
        | Some "on" -> ()
        | _ ->
            let conf = { conf with is_printed_by_template = false } in
            Templ.output_simple conf Templ.Env.empty "buttons_rel");
        if excl_faml = [] then
          Output.printf conf
            {|%s.<br><p><span><a href="%s&m=R&%s">%s</a> %s</span></p>|}
            (([ s1; s2 ] : Adef.safe_string list :> string list)
            |> Util.cftransl conf "no known relationship link between %s and %s"
            |> Utf8.capitalize_fst)
            (Util.commd ~excl:[ "m" ] conf :> string)
            (Util.acces conf base p1 :> string)
            (Util.transl_nth conf "try another/relationship computing" 0
            |> Utf8.capitalize_fst)
            (Util.transl_nth conf "try another/relationship computing" 1)
        else
          Output.printf conf "<ul><li>%s</li><li>%s</li></ul>"
            (s1 : Adef.safe_string :> string)
            (s2 : Adef.safe_string :> string);
        Hutil.trailer conf

let with_generation conf base_str n =
  base_str ^ " "
  ^ Printf.sprintf
      (Util.ftransl conf "of the %s generation")
      (Util.transl_nth conf "nth (generation)" n)
  |> Adef.safe

let parents_label conf base ctx = function
  | 1 -> Util.transl conf "the parents" |> Adef.safe
  | 2 ->
      let txt = Util.transl conf "grand-parents" in
      let is =
        if Util.nb_fields txt = 2 then
          match Relation.get_piece_of_branch conf base ctx (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (Util.pget conf base ip1) = Male then 0 else 1
          | _ -> 0
        else 0
      in
      Util.nth_field txt is |> Adef.safe
  | 3 ->
      let txt = Util.transl conf "great-grand-parents" in
      let is =
        if Util.nb_fields txt = 2 then
          match Relation.get_piece_of_branch conf base ctx (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (Util.pget conf base ip1) = Male then 0 else 1
          | _ -> 0
        else 0
      in
      Util.nth_field txt is |> Adef.safe
  | n -> with_generation conf (Util.transl conf "ancestors (some)") n

let parent_in_law_label conf child_sex parent_sex =
  let txt = Util.transl conf "the father-in-law/the mother-in-law" in
  let is = Util.index_of_sex parent_sex in
  if Util.nb_fields txt = 2 then Util.nth_field txt is |> Adef.safe
  else Util.nth_field txt ((2 * Util.index_of_sex child_sex) + is) |> Adef.safe

let ancestor_label conf base ctx x sex =
  let is = Util.index_of_sex sex in
  match x with
  | 1 -> Util.transl_nth conf "the father/the mother/a parent" is |> Adef.safe
  | 2 ->
      let txt = Util.transl conf "a grandfather/a grandmother/a grandparent" in
      let is =
        if Util.nb_fields txt = 6 then
          match Relation.get_piece_of_branch conf base ctx (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (Util.pget conf base ip1) = Male then is
              else is + 3
          | _ -> is
        else is
      in
      Util.nth_field txt is |> Adef.safe
  | 3 ->
      let txt =
        Util.transl conf
          "a great-grandfather/a great-grandmother/a great-grandparent"
      in
      let is =
        if Util.nb_fields txt = 6 then
          match Relation.get_piece_of_branch conf base ctx (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (Util.pget conf base ip1) = Male then is
              else is + 3
          | _ -> is
        else is
      in
      Util.nth_field txt is |> Adef.safe
  | n -> with_generation conf (Util.transl_nth conf "an ancestor" is) n

let child_in_law_label conf sex_child sex_parent =
  let txt = Util.transl conf "a son-in-law/a daughter-in-law" in
  let is = Util.index_of_sex sex_child in
  if Util.nb_fields txt = 2 then Util.nth_field txt is |> Adef.safe
  else Util.nth_field txt ((2 * Util.index_of_sex sex_parent) + is) |> Adef.safe

let brother_in_law_label conf brother_sex self_sex =
  let txt = Util.transl conf "a brother-in-law/a sister-in-law" in
  let is = Util.index_of_sex brother_sex in
  if Util.nb_fields txt = 2 then Util.nth_field txt is |> Adef.safe
  else Util.nth_field txt ((2 * Util.index_of_sex self_sex) + is) |> Adef.safe

let same_parents conf base p1 p2 =
  Driver.get_parents (Util.pget conf base (Driver.get_iper p1))
  = Driver.get_parents (Util.pget conf base (Driver.get_iper p2))

let cousin_label conf is half a b =
  let key = Printf.sprintf "cousin.%d.%d" a b in
  let pfx = if half then Util.transl_nth conf "half-relationship" is else "" in
  if Hashtbl.mem conf.Config.lexicon key then
    Templ.apply_format conf (Some is) key pfx
  else if a = b then
    Printf.sprintf
      (Util.ftransl_nth conf "a %s%s cousin" is)
      pfx
      (Util.transl_nth conf "nth (cousin)" (a - 1))
  else Templ.apply_format conf (Some is) key pfx

let print_link_name conf base n p1 p2 sol =
  let pp1, pp2, (x1, x2, list), _reltab = sol in
  let is_half =
    x1 > 0 && x2 > 0 && match list with [ _ ] -> true | _ -> false
  in
  if Util.is_hide_names conf p2 && not (Util.authorized_age conf base p2) then
    Output.print_sstring conf "x x"
  else Output.print_string conf @@ Util.person_title_text conf base p2;
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "is");
  if n > 1 then (
    Output.print_sstring conf " ";
    Output.print_sstring conf (Util.transl conf "also");
    Output.print_sstring conf " ")
  else Output.print_sstring conf " ";
  let s, sp1, sp2 =
    let ini_p1 = p1 and ini_p2 = p2 in
    let p1 = match pp1 with Some p1 -> p1 | _ -> p1 in
    let p2 = match pp2 with Some p2 -> p2 | _ -> p2 in
    let sp1 = pp1 <> None in
    let sp2 = pp2 <> None in
    if x2 = 0 && x1 = 1 && sp1 then
      ( parent_in_law_label conf (Driver.get_sex ini_p1) (Driver.get_sex ini_p2),
        false,
        sp2 )
    else if x1 = 0 && x2 = 1 && sp2 then
      ( child_in_law_label conf (Driver.get_sex ini_p2) (Driver.get_sex ini_p1),
        sp1,
        false )
    else if
      x1 = 1 && x2 = 1 && (sp1 || sp2)
      && Driver.get_sex p2 <> Neuter
      && same_parents conf base p2 p1
    then
      ( brother_in_law_label conf (Driver.get_sex ini_p2) (Driver.get_sex ini_p1),
        false,
        false )
    else
      let is = Util.index_of_sex (Driver.get_sex p2) in
      let key = Printf.sprintf "cousin.%d.%d" x1 x2 in
      let label =
        if x1 = 0 || x2 = 0 || x1 = x2 || Hashtbl.mem conf.Config.lexicon key
        then cousin_label conf is is_half x1 x2 |> Adef.safe
        else
          let cous = cousin_label conf is is_half (min x1 x2) (min x1 x2) in
          let diff = abs (x1 - x2) in
          if x2 < x1 then
            let anc =
              cousin_label conf (Util.index_of_sex Neuter) false diff 0
            in
            Util.transl_a_of_gr_eq_gen_lev conf cous anc anc |> Adef.safe
          else
            let desc = cousin_label conf is false 0 diff in
            Util.transl_a_of_gr_eq_gen_lev conf desc cous cous |> Adef.safe
      in
      (label, sp1, sp2)
  in
  let s =
    if sp2 then
      Util.transl_a_of_gr_eq_gen_lev conf
        (Util.transl_nth conf "the spouse"
           (Util.index_of_sex (Driver.get_sex p2)))
        (s : Adef.safe_string :> string)
        (s : Adef.safe_string :> string)
      |> Adef.safe
    else s
  in
  let s =
    if sp1 then
      match pp1 with
      | Some pp1 ->
          let s' =
            Driver.get_sex pp1 |> Util.index_of_sex
            |> Util.transl_nth conf "the spouse"
            |> Adef.safe
          in
          Util.transl_a_of_gr_eq_gen_lev conf
            (s : Adef.safe_string :> string)
            (s' : Adef.safe_string :> string)
            (s' : Adef.safe_string :> string)
          |> Adef.safe
      | None -> s
    else s
  in
  let s1 = "<strong>" ^<^ Util.std_color conf s ^>^ "</strong>" in
  let s2 =
    if Util.is_hide_names conf p1 && not (Util.authorized_age conf base p1) then
      Adef.safe "x x"
    else Util.gen_person_title_text Util.no_reference conf base p1
  in
  let s =
    if x2 < x1 then
      Util.transl_a_of_b conf
        (s1 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
      |> Adef.safe
    else
      Util.transl_a_of_gr_eq_gen_lev conf
        (s1 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
      |> Adef.safe
  in
  Util.translate_eval (s :> string) |> Output.print_sstring conf;
  Output.print_sstring conf ".\n"

let string_of_big_int conf i =
  let sep = Util.transl conf "(thousand separator)" in
  let rec glop i =
    if i = 0 then ""
    else
      let s = glop (i / 1000) in
      if s = "" then string_of_int (i mod 1000)
      else s ^ sep ^ Printf.sprintf "%03d" (i mod 1000)
  in
  glop i

let print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list =
  let img = Util.get_opt conf "im" true in
  Output.print_sstring conf "<ul>";
  List.iter
    (fun (a, n) ->
      Output.print_sstring conf "<li>";
      Output.print_sstring conf "<em>";
      Output.print_sstring conf
        (if n < 0 then "***" else string_of_big_int conf n);
      Output.print_sstring conf " ";
      Output.print_sstring conf
        (Util.transl_nth conf "branch/branches" (if n = 1 then 0 else 1));
      Output.print_sstring conf "</em>\n";
      if not long then
        let propose_dag = n > 1 && n <= 10 in
        let dp1 = match pp1 with Some p -> p | _ -> p1 in
        let dp2 = match pp2 with Some p -> p | _ -> p2 in
        let str =
          Printf.sprintf
            {|:
           <img src="%s/picto_rel_small.png" alt="">
           <a href="%s">%s%s</a></li>|}
            (Util.images_prefix conf :> string)
            (Util.commd ~excl:[ "m"; "l1"; "l2"; "im" ] conf
             ^^^ "m=RL&" ^<^ Util.acces conf base a ^^^ "&l1="
             ^<^ string_of_int x1 ^<^ "&"
             ^<^ Util.acces_n conf base (Adef.escaped "1") dp1
             ^^^ "&l2=" ^<^ string_of_int x2 ^<^ "&"
             ^<^ Util.acces_n conf base (Adef.escaped "2") dp2
             ^^^ (if pp1 = None then Adef.escaped ""
                  else "&" ^<^ Util.acces_n conf base (Adef.escaped "3") p1)
             ^^^ (if pp2 = None then Adef.escaped ""
                  else "&" ^<^ Util.acces_n conf base (Adef.escaped "4") p2)
             ^^^ (if propose_dag then Adef.escaped "&dag=on"
                  else Adef.escaped "")
             ^>^ if img then "" else "&im=0"
              :> string)
            (Util.transl conf "see" |> Utf8.capitalize_fst)
            (if n > 1 && not propose_dag then
               Util.transl conf " the first branch"
             else "")
        in
        Output.print_sstring conf str)
    list;
  Output.print_sstring conf "</ul>"

let print_solution_not_ancestor conf base long p1 p2 sol =
  let img = Util.get_opt conf "im" true in
  let pp1, pp2, (x1, x2, list), reltab = sol in
  Output.print_sstring conf {|<ul class="li_relationship"><li>|};
  Util.transl conf "indeed" |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf "<ul>";
  List.iter
    (fun (a, n) ->
      Output.print_sstring conf "<li>";
      Output.print_string conf (Util.person_title_text conf base a);
      Output.print_sstring conf " ";
      let count =
        Printf.sprintf "(%d %s)" n
          (Util.transl_nth conf "relationship link/relationship links"
             (if n = 1 then 0 else 1))
      in
      (if long then Output.printf conf {|<em>%s</em>|} count
       else
         let propose_dag = n > 1 && n <= 10 in
         let dp1 = match pp1 with Some p -> p | _ -> p1 in
         let dp2 = match pp2 with Some p -> p | _ -> p2 in
         let href =
           Util.commd ~excl:[ "m"; "l1"; "l2"; "im" ] conf
           ^^^ "m=RL&" ^<^ Util.acces conf base a ^^^ "&l1="
           ^<^ string_of_int x1 ^<^ "&"
           ^<^ Util.acces_n conf base (Adef.escaped "1") dp1
           ^^^ "&l2=" ^<^ string_of_int x2 ^<^ "&"
           ^<^ Util.acces_n conf base (Adef.escaped "2") dp2
           ^^^ (if pp1 = None then Adef.escaped ""
                else "&" ^<^ Util.acces_n conf base (Adef.escaped "3") p1)
           ^^^ (if pp2 = None then Adef.escaped ""
                else "&" ^<^ Util.acces_n conf base (Adef.escaped "4") p2)
           ^^^ (if propose_dag then Adef.escaped "&dag=on" else Adef.escaped "")
           ^>^ if img then "" else "&im=0"
         in
         Output.printf conf {|<a href="%s"><em>%s</em></a>|}
           (href :> string)
           count);
      Output.print_sstring conf "</li>")
    list;
  Output.print_sstring conf "</ul>";
  let is_are =
    match list with
    | [ _ ] -> Util.transl conf "is"
    | _ -> Util.transl conf "are"
  in
  Output.print_sstring conf is_are;
  Output.print_sstring conf " ";
  Output.print_sstring conf (Util.transl conf "at the same time");
  let lab proj x =
    let key, is =
      match list with
      | [ (a, _) ] ->
          (Printf.sprintf "cousin.%d.0" x, Util.index_of_sex (Driver.get_sex a))
      | _ -> (Printf.sprintf "cousins.%d.0" x, 0)
    in
    if Hashtbl.mem conf.Config.lexicon key then
      Util.transl_nth conf key is |> Adef.safe
    else
      let ctx =
        {
          Relation.pos = { Relation.reltab; ancestors = list; degree = x };
          proj;
        }
      in
      match list with
      | [ (a, _) ] -> ancestor_label conf base ctx x (Driver.get_sex a)
      | _ -> parents_label conf base ctx x
  in
  let print pp p (alab : Adef.safe_string) =
    let s = Util.gen_person_title_text Util.no_reference conf base p in
    let s =
      match pp with
      | None ->
          Util.transl_a_of_b conf (alab :> string) (s :> string) (s :> string)
      | Some pp ->
          Util.transl_a_of_gr_eq_gen_lev conf
            (let s =
               Util.transl_nth conf "the spouse"
                 (Util.index_of_sex (Driver.get_sex pp))
             in
             Util.transl_a_of_b conf (alab :> string) s s)
            (s :> string)
            (s :> string)
    in
    Output.print_sstring conf (Util.translate_eval s)
  in
  Output.print_sstring conf "<ul><li>";
  print pp1 p1 (lab (fun r -> r.Consang.lens1) x1);
  Output.print_sstring conf "</li><li>";
  print pp2 p2 (lab (fun r -> r.Consang.lens2) x2);
  Output.print_sstring conf "</li></ul></li></ul>"

let print_solution conf base long n p1 p2 sol =
  let pp1, pp2, (x1, x2, list), _ = sol in
  Output.print_sstring conf {|<div class="mt-3">&#9654;&nbsp;|};
  print_link_name conf base n p1 p2 sol;
  Output.print_sstring conf "</div>\n";
  if x1 = 0 || x2 = 0 then
    print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list
  else print_solution_not_ancestor conf base long p1 p2 sol;
  Output.print_sstring conf "\n"

let max_br = 33

let print_dag_links conf base p1 p2 rl =
  let sps = Util.get_opt conf "sp" true in
  let img = Util.get_opt conf "im" true in
  let anc_map =
    List.fold_left
      (fun anc_map (pp1, pp2, (x1, x2, list), _) ->
        List.fold_left
          (fun anc_map (p, n) ->
            let pp1, pp2, nn, nt, maxlev =
              Option.value ~default:(pp1, pp2, 0, 0, 0)
                (Iper.Map.find_opt (Driver.get_iper p) anc_map)
            in
            if nn >= max_br then anc_map
            else
              let v = (pp1, pp2, nn + n, nt + 1, max maxlev (max x1 x2)) in
              Iper.Map.add (Driver.get_iper p) v anc_map)
          anc_map list)
      Iper.Map.empty rl
  in
  let is_anc =
    match rl with (_, _, (x1, x2, _), _) :: _ -> x1 = 0 || x2 = 0 | _ -> false
  in
  let something =
    Iper.Map.fold
      (fun _ (_, _, nn, nt, _) something ->
        something || (nt > 1 && nn > 1 && nn < max_br))
      anc_map false
  in
  if something then (
    let rest = ref false in
    if is_anc then (
      Output.print_sstring conf {|<img src="|};
      Output.print_sstring conf (Util.images_prefix conf);
      Output.print_sstring conf {|/picto_fleche_bleu.png" alt="">|})
    else Output.print_sstring conf "<ul>";
    Iper.Map.iter
      (fun ip (pp1, pp2, nn, nt, _) ->
        let dp1 = match pp1 with Some p -> p | _ -> p1 in
        let dp2 = match pp2 with Some p -> p | _ -> p2 in
        if nt > 1 && nn > 1 && nn < max_br then (
          let a = Util.pget conf base ip in
          if not is_anc then (
            Output.print_sstring conf "<li>";
            Output.print_string conf (Util.person_title_text conf base a);
            Output.print_sstring conf (Util.transl conf ":");
            Output.print_sstring conf " ");
          Output.print_sstring conf "<a href=\"";
          Output.print_string conf
            (Util.commd ~excl:[ "m"; "em"; "ei"; "et" ] conf);
          Output.print_sstring conf "m=RL&";
          Output.print_string conf (Util.acces conf base a);
          Output.print_sstring conf "&";
          Output.print_string conf
            (Util.acces_n conf base (Adef.escaped "1") dp1);
          Output.print_sstring conf "&";
          Output.print_string conf
            (Util.acces_n conf base (Adef.escaped "2") dp2);
          if pp1 <> None then (
            Output.print_sstring conf "&";
            Output.print_string conf
              (Util.acces_n conf base (Adef.escaped "3") p1));
          if pp2 <> None then (
            Output.print_sstring conf "&";
            Output.print_string conf
              (Util.acces_n conf base (Adef.escaped "4") p2));
          let l1, l2 =
            List.fold_left
              (fun (l1, l2) (_, _, (x1, x2, list), _) ->
                List.fold_left
                  (fun (l1, l2) (a, _) ->
                    if Driver.get_iper a = ip then
                      let l1 = if List.mem x1 l1 then l1 else x1 :: l1 in
                      let l2 = if List.mem x2 l2 then l2 else x2 :: l2 in
                      (l1, l2)
                    else (l1, l2))
                  (l1, l2) list)
              ([], []) rl
          in
          Output.print_sstring conf "&l1=";
          List.iteri
            (fun i x ->
              if i <> 0 then Output.print_sstring conf ",";
              Output.print_sstring conf (string_of_int x))
            l1;
          Output.print_sstring conf "&l2=";
          List.iteri
            (fun i x ->
              if i <> 0 then Output.print_sstring conf ",";
              Output.print_sstring conf (string_of_int x))
            l2;
          if not img then Output.print_sstring conf "&im=0";
          if not sps then Output.print_sstring conf "&sp=0";
          Output.print_sstring conf {|&dag=on">|};
          if is_anc then Output.print_sstring conf (Util.transl conf "tree")
          else (
            Output.print_sstring conf (string_of_int nn);
            Output.print_sstring conf " ";
            Output.print_sstring conf
              (Util.transl_nth conf "relationship link/relationship links" 1));
          Output.print_sstring conf "</a>";
          if not is_anc then Output.print_sstring conf "</li>" else rest := true))
      anc_map;
    if !rest then Output.print_sstring conf "<li>...</li>";
    if is_anc then Output.print_sstring conf "\n"
    else Output.print_sstring conf "</ul>\n")

let print_propose_upto conf base p1 p2 rl =
  match rl with
  | (None, None, (x1, x2, _), _) :: _ when x1 = 0 || x2 = 0 ->
      let maxlen =
        List.fold_right
          (fun (_, _, (x1, x2, _), _) maxlen -> max maxlen (max x1 x2))
          rl 0
      in
      let p, a = if x1 = 0 then (p2, p1) else (p1, p2) in
      let s =
        (Util.person_title_text conf base p : Adef.safe_string :> string)
      in
      let str =
        Printf.sprintf
          {|
          <div>
            <img src="%s/picto_rel_asc.png" alt="">
            <a href="%s%s&m=A&t=X&%s&l=%s">%s %s</a>.
          </div>
        |}
          (Util.images_prefix conf :> string)
          (Util.commd ~excl:[ "m"; "t"; "l" ] conf :> string)
          (Util.acces conf base p :> string)
          (Util.acces_n conf base (Adef.escaped "1") a :> string)
          (string_of_int maxlen)
          (Util.transl_a_of_b conf
             (Util.transl_nth conf "ancestor/ancestors" 1)
             s s
          |> Util.translate_eval |> Utf8.capitalize_fst)
          ((Util.person_title_text conf base a : Adef.safe_string :> string)
          |> Util.transl_decline conf "up to")
      in
      Output.print_sstring conf str
  | _ -> ()

let print_one_path conf base found a p1 p2 pp1 pp2 l1 l2 =
  let ip = Driver.get_iper a in
  let sp1 = match pp1 with Some _ -> Some p1 | _ -> None in
  let sp2 = match pp2 with Some _ -> Some p2 | _ -> None in
  let p1 = match pp1 with Some p1 -> p1 | _ -> p1 in
  let p2 = match pp2 with Some p2 -> p2 | _ -> p2 in
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  let dist = RelationLink.make_dist_tab conf base ip (max l1 l2 + 1) in
  let b1 = RelationLink.find_first_branch conf base dist ip l1 ip1 Neuter in
  let b2 = RelationLink.find_first_branch conf base dist ip l2 ip2 Neuter in
  match (b1, b2) with
  | Some b1, Some b2 ->
      let info =
        RelationLink.
          {
            ip;
            sp = Driver.get_sex a;
            ip1;
            ip2;
            b1;
            b2;
            c1 = 1;
            c2 = 1;
            pb1 = None;
            pb2 = None;
            nb1 = None;
            nb2 = None;
            sp1;
            sp2;
          }
      in
      if not (List.mem (b1, b2) !found) then (
        Output.print_sstring conf
          {|<table width="100%"><tr><td align="center"><table><tr><td>|};
        RelationLink.print_relation_path conf base info;
        Output.print_sstring conf {|</td></tr></table></td></tr></table>|};
        found := (b1, b2) :: !found)
  | _ -> ()

let print_path conf base p1 p2 (pp1, pp2, (l1, l2, list), _) =
  let found = ref [] in
  List.iter
    (fun (a, _) -> print_one_path conf base found a p1 p2 pp1 pp2 l1 l2)
    list;
  Output.print_sstring conf "\n"

let print_main_relationship conf base long p1 p2 rel =
  let total =
    match rel with None -> Sosa.zero | Some (_, total, _) -> total
  in
  let title _ =
    Util.transl conf "relationship"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    if not (Sosa.eq total Sosa.zero) then (
      Output.print_sstring conf " (";
      Output.print_sstring conf
        (Sosa.to_string_sep (Util.transl conf "(thousand separator)") total);
      Output.print_sstring conf " ";
      (if Sosa.eq total Sosa.one then 0 else 1)
      |> Util.transl_nth conf "relationship link/relationship links"
      |> Output.print_sstring conf;
      Output.print_sstring conf ")")
  in
  Hutil.header conf title;
  (match Util.p_getenv conf.env "cgl" with
  | Some "on" -> ()
  | _ ->
      let conf = { conf with is_printed_by_template = false } in
      Templ.output_simple conf Templ.Env.empty "buttons_rel");
  let extras =
    List.filter_map
      (fun x -> x)
      [
        (match
           (Util.p_getenv conf.env "sp", Util.p_getenv conf.env "spouse")
         with
        | Some ("off" | "0"), _ | _, Some "off" -> Some ("sp", Mutil.encode "0")
        | _ -> None);
        (match Util.p_getenv conf.env "cgl" with
        | Some "on" -> Some ("cgl", Mutil.encode "on")
        | _ -> None);
      ]
  in
  conf.senv <- List.rev_append (List.rev conf.senv) extras;
  (match rel with
  | None ->
      if Driver.get_iper p1 = Driver.get_iper p2 then (
        Util.transl conf "it is the same person!"
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf " ")
      else
        Output.printf conf
          {|%s.<br><p><span><a href="%s&m=R&%s">%s</a> %s.</span></p>|}
          (([
              Util.gen_person_title_text Util.reference conf base p1;
              Util.gen_person_title_text Util.reference conf base p2;
            ]
             : Adef.safe_string list
             :> string list)
          |> Util.cftransl conf "no known relationship link between %s and %s"
          |> Utf8.capitalize_fst)
          (Util.commd ~excl:[ "m" ] conf :> string)
          (Util.acces conf base p1 :> string)
          (Util.transl_nth conf "try another/relationship computing" 0
          |> Utf8.capitalize_fst)
          (Util.transl_nth conf "try another/relationship computing" 1)
  | Some (rl, _, relationship) ->
      let a1 = p1 in
      let a2 = p2 in
      let all_by_marr =
        List.for_all
          (function Some _, _, _, _ | _, Some _, _, _ -> true | _ -> false)
          rl
      in
      List.iteri
        (fun i sol ->
          let i = i + 1 in
          print_solution conf base long i p1 p2 sol;
          if long then print_path conf base p1 p2 sol)
        rl;
      Output.print_sstring conf "\n";
      if long then () else print_dag_links conf base p1 p2 rl;
      (if
         (not all_by_marr)
         && Util.authorized_age conf base p1
         && Util.authorized_age conf base p2
         && Driver.get_consang a1 != Adef.fix (-1)
         && Driver.get_consang a2 != Adef.fix (-1)
       then
         let pct =
           Adef.float_of_fix (Adef.fix_of_float relationship) *. 100.0
         in
         let pct_dna = pct *. 2.0 in
         let colon = Util.transl conf ":" in
         Output.printf conf {|<div>%s (Malécot)%s %s%%<br>%s%s %s%%</div>|}
           (Utf8.capitalize_fst
              (Util.transl_nth conf "relationship coefficient/percentage" 0))
           colon
           (Util.string_of_decimal_num conf (round_2_dec pct))
           (Utf8.capitalize_fst
              (Util.transl conf "estimated average percentage of shared DNA"))
           colon
           (Util.string_of_decimal_num conf (round_2_dec pct_dna)));
      print_propose_upto conf base p1 p2 rl);
  Hutil.trailer conf

let multi_relation_next_txt conf pl2 lim
    (assoc_txt : (Geneweb_db.Driver.iper, string) Hashtbl.t) =
  match pl2 with
  | [] -> Adef.escaped ""
  | _ ->
      let acc =
        Adef.escaped (if lim > 0 then "&lim=" ^ string_of_int lim else "")
      in
      let acc =
        List.fold_left
          (fun (acc, n) p ->
            let acc =
              match Hashtbl.find_opt assoc_txt (Driver.get_iper p) with
              | Some txt ->
                  "&t" ^<^ string_of_int n ^<^ "="
                  ^<^ (Mutil.encode txt :> Adef.escaped_string)
                  ^^^ acc
              | None -> acc
            in
            let acc =
              "&i" ^<^ string_of_int n ^<^ "="
              ^<^ (Driver.get_iper p |> Driver.Iper.to_string |> Mutil.encode
                    :> Adef.escaped_string)
              ^^^ acc
            in
            (acc, n - 1))
          (acc, List.length pl2)
          (List.rev pl2)
        |> fst
      in
      Util.commd ~excl:[ "t"; "i"; "m" ] conf ^^^ "m=RLM" ^<^ acc

let print_no_relationship conf base pl =
  let title _ =
    Util.transl conf "tree" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul>";
  List.iter
    (fun p ->
      Output.print_sstring conf "<li>";
      Output.print_string conf (Util.referenced_person_title_text conf base p);
      Output.print_sstring conf "</li>")
    pl;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_multi_relation conf base pl lim
    (assoc_txt : (Geneweb_db.Driver.iper, string) Hashtbl.t) =
  let pl1, pl2 =
    if lim <= 0 then (pl, [])
    else
      let rec loop n rev_pl1 pl2 =
        match (n, pl2) with
        | _, [] | _, [ _ ] -> (pl, [])
        | 0, _ -> (
            match rev_pl1 with
            | p :: _ -> (List.rev rev_pl1, p :: pl2)
            | _ -> (pl, []))
        | n, p :: pl -> loop (n - 1) (p :: rev_pl1) pl
      in
      loop lim [] pl
  in
  let merge path path1 =
    match path with
    | [] -> path1
    | _ -> (
        match List.rev path1 with
        | _ :: tl -> List.rev_append tl path
        | [] -> path)
  in
  let linked = ref Iper.Set.empty in
  let note path1 =
    List.iter (fun (ip, _) -> linked := Iper.Set.add ip !linked) path1
  in
  let path =
    let rec loop path anchor = function
      | [] -> path
      | p :: rest -> (
          match anchor with
          | None -> loop path (Some p) rest
          | Some a -> (
              match
                Relation.get_shortest_path_relation conf base
                  (Driver.get_iper a) (Driver.get_iper p) []
              with
              | Some (path1, _) ->
                  note path1;
                  loop (merge path path1) (Some p) rest
              | None -> loop path anchor rest))
    in
    loop [] None pl1
  in
  let path =
    match pl1 with
    | first :: (_ :: _ as tl) -> (
        let last = List.fold_left (fun _ p -> p) first tl in
        let ipf = Driver.get_iper first and ipl = Driver.get_iper last in
        if Iper.Set.mem ipf !linked && Iper.Set.mem ipl !linked then path
        else
          match Relation.get_shortest_path_relation conf base ipl ipf [] with
          | Some (path1, _) ->
              note path1;
              merge path path1
          | None -> path)
    | _ -> path
  in
  (* Persons reached by no path become standalone nodes, prepended so
     dag_ind_list_of_path, folding the reversed path, processes them last
     and leaves the connected chain's prev_ind intact. *)
  let path =
    List.fold_left
      (fun path p ->
        let ip = Driver.get_iper p in
        if Iper.Set.mem ip !linked then path else (ip, Relation.Self) :: path)
      path pl1
  in
  if path = [] then print_no_relationship conf base pl
  else
    let elem_txt p =
      let content =
        match Hashtbl.find_opt assoc_txt (Driver.get_iper p) with
        | Some txt when txt <> "" ->
            "<b>(" ^<^ (Util.escape_html txt :> Adef.safe_string) ^>^ ")</b>"
        | _ -> Adef.safe ""
      in
      DagDisplay.Item (p, content)
    in
    let vbar_txt _ = Adef.escaped "" in
    let next_txt = multi_relation_next_txt conf pl2 lim assoc_txt in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print_base_loop conf base p =
  let title _ =
    Util.transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.printf conf
    (Util.fcapitale
       (Util.ftransl conf "loop in database: %s is his/her own ancestor"))
    (Util.update_family_loop conf base p
       (Util.designation base p : Adef.escaped_string :> Adef.safe_string)
      :> string);
  Output.print_sstring conf ".";
  Hutil.trailer conf

let print conf base p p1 =
  match p1 with
  | Some p1 -> (
      match Util.p_getenv conf.env "et" with
      | Some "S" -> print_shortest_path conf base p1 p
      | x -> (
          let by_marr = x = Some "M" in
          let long =
            match Util.p_getenv conf.env "long" with
            | Some "on" -> true
            | _ -> false
          in
          match
            try Left (Relation.compute_relationship conf base by_marr p1 p)
            with Consang.TopologicalSortError p -> Right p
          with
          | Left rel -> print_main_relationship conf base long p1 p rel
          | Right p -> print_base_loop conf base p))
  | None -> Perso.interp_templ "relmenu" conf base p

let print_multi conf base =
  let assoc_txt : (Geneweb_db.Driver.iper, string) Hashtbl.t =
    Hashtbl.create 53
  in
  if Util.url_has_pnoc_params conf.env then
    let clean_url =
      Util.normalize_person_pool_url conf base "RLM" (Some assoc_txt)
    in
    Server.http_redirect_temporarily clean_url
  else
    let pl =
      let rec loop pl i =
        let k = string_of_int i in
        match Util.find_person_in_env conf base k with
        | Some p ->
            (match Util.p_getenv conf.env ("t" ^ k) with
            | Some x -> Hashtbl.add assoc_txt (Driver.get_iper p) x
            | None -> ());
            loop (p :: pl) (i + 1)
        | None -> List.rev pl
      in
      loop [] 1
    in
    let lim = Option.value ~default:0 (Util.p_getint conf.env "lim") in
    print_multi_relation conf base pl lim assoc_txt
