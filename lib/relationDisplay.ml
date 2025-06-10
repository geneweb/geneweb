(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open Dag2html
open Relation
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver

let dag_of_ind_dag_list indl =
  let indl, _ =
    List.fold_right
      (fun ind (indl, cnt) -> ((ind, cnt) :: indl, cnt + 1))
      indl ([], 0)
  in
  let idag_of_di_ind ind = idag_of_int (List.assq ind indl) in
  List.map
    (fun (ind, cnt) ->
      {
        pare = List.map idag_of_di_ind ind.di_famc.df_pare;
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
  let d = { dag = Array.of_list (List.rev nl) } in
  let set =
    List.fold_left
      (fun set n ->
        match n.valu with
        | Def.Left ip -> Dag.Pset.add ip set
        | Def.Right _ -> set)
      Dag.Pset.empty nl
  in
  (set, d)

let old_print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  let invert = Util.p_getenv conf.env "invert" = Some "on" in
  let set, d = dag_of_relation_path conf base path in
  let page_title =
    transl conf "relationship" |> Utf8.capitalize_fst |> Adef.safe
  in
  let hts =
    DagDisplay.make_tree_hts conf base elem_txt vbar_txt invert set [] d
  in
  DagDisplay.print_slices_menu_or_dag_page conf base page_title hts next_txt

let print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  if p_getenv conf.env "new" <> Some "on" then
    old_print_relationship_dag conf base elem_txt vbar_txt path next_txt
  else
    let invert = Util.p_getenv conf.env "invert" = Some "on" in
    let set = ind_set_of_relation_path base path in
    let page_title =
      transl conf "relationship" |> Utf8.capitalize_fst |> Adef.safe
    in
    DagDisplay.make_and_print_dag conf base elem_txt vbar_txt invert set []
      page_title next_txt

let next_relation_link_txt conf ip1 ip2 excl_faml : Adef.escaped_string =
  let sps =
    match (Util.p_getenv conf.env "sp", Util.p_getenv conf.env "spouse") with
    | Some ("off" | "0"), _ | _, Some "off" -> false
    | _, _ -> true
  in
  let bd =
    match p_getenv conf.env "bd" with
    | None | Some ("0" | "") -> Adef.escaped ""
    | Some x -> "&bd=" ^<^ (Mutil.encode x :> Adef.escaped_string)
  in
  let color =
    match p_getenv conf.env "color" with
    | None -> Adef.escaped ""
    | Some x -> "&color=" ^<^ (Mutil.encode x :> Adef.escaped_string)
  in
  let sl, _ =
    List.fold_left
      (fun (sl, i) ifam ->
        ("&ef" ^ string_of_int i ^ "=" ^ Driver.Ifam.to_string ifam ^ sl, i - 1))
      ("", List.length excl_faml - 1)
      excl_faml
  in
  commd conf ^^^ "em=R&ei=" ^<^ Driver.Iper.to_string ip1 ^<^ "&i="
  ^<^ Driver.Iper.to_string ip2
  ^<^ (if sps then "" else "&sp=0")
  ^<^ bd ^^^ color ^>^ "&et=S" ^ sl

let print_relation_path conf base ip1 ip2 path ifam excl_faml =
  if path = [] then (
    let title _ =
      transl conf "relationship" |> Utf8.capitalize_fst
      |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Hutil.trailer conf)
  else
    let next_txt = next_relation_link_txt conf ip1 ip2 (ifam :: excl_faml) in
    let elem_txt p = DagDisplay.Item (p, Adef.safe "") in
    let vbar_txt ip =
      let u = pget conf base ip in
      let excl_faml = Array.to_list (Driver.get_family u) @ excl_faml in
      next_relation_link_txt conf ip1 ip2 excl_faml
    in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print_shortest_path conf base p1 p2 =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  if ip1 = ip2 then (
    let title _ =
      transl conf "relationship" |> Utf8.capitalize_fst
      |> Output.print_sstring conf
    in
    Hutil.header conf title;
    transl conf "it is the same person!"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf "\n";
    Hutil.trailer conf)
  else
    let excl_faml = excl_faml conf base in
    let title _ =
      transl conf "relationship" |> Utf8.capitalize_fst
      |> Output.print_sstring conf
    in
    match get_shortest_path_relation conf base ip1 ip2 excl_faml with
    | Some (path, ifam) ->
        print_relation_path conf base ip1 ip2 path ifam excl_faml
    | None ->
        let s1 = gen_person_title_text reference conf base p1 in
        let s2 = gen_person_title_text reference conf base p2 in
        Hutil.header conf title;
        (match p_getenv conf.env "cgl" with
        | Some "on" -> ()
        | _ ->
            let conf = { conf with is_printed_by_template = false } in
            Templ.output_simple conf Templ.Env.empty "buttons_rel");
        if excl_faml = [] then (
          ([ s1; s2 ] : Adef.safe_string list :> string list)
          |> cftransl conf "no known relationship link between %s and %s"
          |> Utf8.capitalize_fst |> Output.print_sstring conf;
          Output.print_sstring conf ".<br><p><span><a href=\"";
          Output.print_string conf (commd conf);
          Output.print_sstring conf "&m=R&";
          Output.print_string conf (acces conf base p1);
          Output.print_sstring conf "\">";
          transl_nth conf "try another/relationship computing" 0
          |> Utf8.capitalize_fst |> Output.print_sstring conf;
          Output.print_sstring conf "</a> ";
          transl_nth conf "try another/relationship computing" 1
          |> Output.print_sstring conf;
          Output.print_sstring conf "</span></p>")
        else (
          Output.print_sstring conf "<ul><li>";
          Output.print_string conf s1;
          Output.print_sstring conf "</li><li>";
          Output.print_string conf s2;
          Output.print_sstring conf "</ul>");
        Hutil.trailer conf

let parents_label conf base info = function
  | 1 -> transl conf "the parents" |> Adef.safe
  | 2 ->
      let txt = transl conf "grand-parents" in
      let is =
        if nb_fields txt = 2 then
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then 0 else 1
          | _ -> (* must be a bug *) 0
        else 0
      in
      nth_field txt is |> Adef.safe
  | 3 ->
      let txt = transl conf "great-grand-parents" in
      let is =
        if nb_fields txt = 2 then
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then 0 else 1
          | _ -> (* must be a bug *) 0
        else 0
      in
      nth_field txt is |> Adef.safe
  | n ->
      transl conf "ancestors (some)"
      ^ " "
      ^ Printf.sprintf
          (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n)
      |> Adef.safe

let parent_in_law_label conf child_sex parent_sex =
  let txt = transl conf "the father-in-law/the mother-in-law" in
  let is = index_of_sex parent_sex in
  if nb_fields txt = 2 then nth_field txt is |> Adef.safe
  else nth_field txt ((2 * index_of_sex child_sex) + is) |> Adef.safe

let ancestor_label conf base info x sex =
  let is = index_of_sex sex in
  match x with
  | 1 -> transl_nth conf "the father/the mother/a parent" is |> Adef.safe
  | 2 ->
      let txt = transl conf "a grandfather/a grandmother/a grandparent" in
      let is =
        if nb_fields txt = 6 then
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then is else is + 3
          | _ -> (* must be a bug *) is
        else is
      in
      nth_field txt is |> Adef.safe
  | 3 ->
      let txt =
        transl conf
          "a great-grandfather/a great-grandmother/a great-grandparent"
      in
      let is =
        if nb_fields txt = 6 then
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then is else is + 3
          | _ -> (* must be a bug *) is
        else is
      in
      nth_field txt is |> Adef.safe
  | n ->
      transl_nth conf "an ancestor" is
      ^ " "
      ^ Printf.sprintf
          (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n)
      |> Adef.safe

let child_in_law_label conf sex_child sex_parent =
  let txt = transl conf "a son-in-law/a daughter-in-law" in
  let is = index_of_sex sex_child in
  if nb_fields txt = 2 then nth_field txt is |> Adef.safe
  else nth_field txt ((2 * index_of_sex sex_parent) + is) |> Adef.safe

let descendant_label conf base info x p =
  let is = index_of_sex (Driver.get_sex p) in
  match x with
  | 1 -> transl_nth conf "a son/a daughter/a child" is |> Adef.safe
  | 2 ->
      let txt = transl conf "a grandson/a granddaughter/a grandchild" in
      let is =
        if nb_fields txt = 6 then
          let info = (info, fun r -> r.Consang.lens2) in
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then is else is + 3
          | _ -> (* must be a bug *) is
        else is
      in
      nth_field txt is |> Adef.safe
  | 3 ->
      let txt =
        transl conf "a great-grandson/a great-granddaughter/a great-grandchild"
      in
      let is =
        if nb_fields txt = 12 then
          let info = (info, fun r -> r.Consang.lens2) in
          match get_piece_of_branch conf base info (1, 2) with
          | [ ip1; ip2 ] ->
              let is =
                if Driver.get_sex (pget conf base ip1) = Male then is
                else is + 6
              in
              if Driver.get_sex (pget conf base ip2) = Male then is else is + 3
          | _ -> (* must be a bug *) is
        else is
      in
      nth_field txt is |> Adef.safe
  | n ->
      transl_nth conf "a descendant" is
      ^ " "
      ^ Printf.sprintf
          (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n)
      |> Adef.safe

(* transl_nth does a plain translation *)
(* apply_format handles %s, %d after the nth selection *)
let brother_label conf x sex : Adef.safe_string =
  let is = index_of_sex sex in
  let str =
    match x with
    | 1 -> Templ.apply_format conf (Some is) "a brother/a sister/a sibling" ""
    | 2 -> Templ.apply_format conf (Some is) "a cousin" ""
    | 3 -> Templ.apply_format conf (Some is) "a 2nd cousin" ""
    | 4 -> Templ.apply_format conf (Some is) "a 3rd cousin" ""
    | n ->
        Printf.sprintf
          (ftransl_nth conf "a %s cousin" is)
          (transl_nth conf "nth (cousin)" (n - 1))
  in
  Adef.safe str

let half_brother_label conf sex =
  let is = index_of_sex sex in
  Templ.apply_format conf (Some is)
    "a half-brother/a half-sister/a half-sibling" ""
  |> Adef.safe

let brother_in_law_label conf brother_sex self_sex =
  let txt = transl conf "a brother-in-law/a sister-in-law" in
  let is = index_of_sex brother_sex in
  if nb_fields txt = 2 then nth_field txt is |> Adef.safe
  else nth_field txt ((2 * index_of_sex self_sex) + is) |> Adef.safe

let uncle_label conf base info x p =
  let is = index_of_sex (Driver.get_sex p) in
  match x with
  | 1 ->
      let txt = "an uncle/an aunt" in
      let is =
        if nb_fields txt = 4 then
          let info = (info, fun r -> r.Consang.lens1) in
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then is else is + 2
          | _ -> (* must be a bug *) is
        else is
      in
      Templ.apply_format conf (Some is) txt "" |> Adef.safe
  | 2 ->
      let txt = "a great-uncle/a great-aunt" in
      let is =
        if nb_fields txt = 4 then
          let info = (info, fun r -> r.Consang.lens1) in
          match get_piece_of_branch conf base info (1, 1) with
          | [ ip1 ] ->
              if Driver.get_sex (pget conf base ip1) = Male then is else is + 2
          | _ -> (* must be a bug *) is
        else is
      in
      Templ.apply_format conf (Some is) txt "" |> Adef.safe
  | n ->
      Templ.apply_format conf (Some is) "an uncle/an aunt" ""
      ^ " "
      ^ Printf.sprintf
          (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n)
      |> Adef.safe

let nephew_label conf x p =
  let is = index_of_sex (Driver.get_sex p) in
  match x with
  | 1 -> Templ.apply_format conf (Some is) "a nephew/a niece" "" |> Adef.safe
  | 2 ->
      Templ.apply_format conf (Some is) "a great-nephew/a great-niece" ""
      |> Adef.safe
  | n ->
      Templ.apply_format conf (Some is) "a nephew/a niece" ""
      ^ " "
      ^ Printf.sprintf
          (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n)
      |> Adef.safe

let same_parents conf base p1 p2 =
  Driver.get_parents (pget conf base (Driver.get_iper p1))
  = Driver.get_parents (pget conf base (Driver.get_iper p2))

let print_link_name conf base n p1 p2 sol =
  let pp1, pp2, (x1, x2, list), reltab = sol in
  let info = (reltab, list) in
  if is_hide_names conf p2 && not (authorized_age conf base p2) then
    Output.print_sstring conf "x x"
  else Output.print_string conf @@ person_title_text conf base p2;
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "is");
  if n > 1 then (
    Output.print_sstring conf " ";
    Output.print_sstring conf (transl conf "also");
    Output.print_sstring conf " ")
  else Output.print_sstring conf " ";
  let s, sp1, sp2 =
    let ini_p1 = p1 and ini_p2 = p2 in
    let p1 = match pp1 with Some p1 -> p1 | _ -> p1 in
    let p2 = match pp2 with Some p2 -> p2 | _ -> p2 in
    let sp1 = pp1 <> None in
    let sp2 = pp2 <> None in
    if x2 = 0 then
      if sp1 && x1 = 1 then
        ( parent_in_law_label conf (Driver.get_sex ini_p1)
            (Driver.get_sex ini_p2),
          false,
          sp2 )
      else
        let info = ((info, x1), fun r -> r.Consang.lens1) in
        (ancestor_label conf base info x1 (Driver.get_sex p2), sp1, sp2)
    else if x1 = 0 then
      if sp2 && x2 = 1 then
        ( child_in_law_label conf (Driver.get_sex ini_p2) (Driver.get_sex ini_p1),
          sp1,
          false )
      else (descendant_label conf base (info, x2) x2 p2, sp1, sp2)
    else if x2 = x1 then
      if x2 = 1 && not (same_parents conf base p2 p1) then
        (half_brother_label conf (Driver.get_sex p2), sp1, sp2)
      else if x2 = 1 && (sp2 || sp1) && Driver.get_sex p2 <> Neuter then
        ( brother_in_law_label conf (Driver.get_sex ini_p2)
            (Driver.get_sex ini_p1),
          false,
          false )
      else (brother_label conf x1 (Driver.get_sex p2), sp1, sp2)
    else if x2 = 1 then (uncle_label conf base (info, x1) (x1 - x2) p2, sp1, sp2)
    else if x1 = 1 then (nephew_label conf (x2 - x1) p2, sp1, sp2)
    else if x2 < x1 then
      let s =
        let info = ((info, x1), fun r -> r.Consang.lens1) in
        let s = ancestor_label conf base info (x1 - x2) Neuter in
        transl_a_of_gr_eq_gen_lev conf
          (brother_label conf x2 (Driver.get_sex p2)
            : Adef.safe_string
            :> string)
          (s : Adef.safe_string :> string)
          (s : Adef.safe_string :> string)
        |> Adef.safe
      in
      (s, sp1, sp2)
    else
      let s =
        let sm = brother_label conf x1 Male in
        let sf = brother_label conf x1 Female in
        let d = descendant_label conf base (info, x2) (x2 - x1) p2 in
        let s =
          if sm = sf then sm
          else
            let info = ((info, x2), fun r -> r.Consang.lens2) in
            match get_piece_of_branch conf base info (x2 - x1, x2 - x1) with
            | [ ip2 ] ->
                if Driver.get_sex (pget conf base ip2) = Male then sm else sf
            | _ -> sm
        in
        transl_a_of_gr_eq_gen_lev conf
          (d : Adef.safe_string :> string)
          (s : Adef.safe_string :> string)
          (s : Adef.safe_string :> string)
        |> Adef.safe
      in
      (s, sp1, sp2)
  in
  let s =
    if sp2 then
      transl_a_of_gr_eq_gen_lev conf
        (transl_nth conf "the spouse" (index_of_sex (Driver.get_sex p2)))
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
            Driver.get_sex pp1 |> index_of_sex
            |> transl_nth conf "the spouse"
            |> Adef.safe
          in
          transl_a_of_gr_eq_gen_lev conf
            (s : Adef.safe_string :> string)
            (s' : Adef.safe_string :> string)
            (s' : Adef.safe_string :> string)
          |> Adef.safe
      | None -> s
    else s
  in
  let s1 = "<strong>" ^<^ std_color conf s ^>^ "</strong>" in
  let s2 =
    if is_hide_names conf p1 && not (authorized_age conf base p1) then
      Adef.safe "x x"
    else gen_person_title_text no_reference conf base p1
  in
  let s =
    if x2 < x1 then
      transl_a_of_b conf
        (s1 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
      |> Adef.safe
    else
      transl_a_of_gr_eq_gen_lev conf
        (s1 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
        (s2 : Adef.safe_string :> string)
      |> Adef.safe
  in
  Util.translate_eval (s :> string) |> Output.print_sstring conf;
  Output.print_sstring conf ".\n"

let string_of_big_int conf i =
  let sep = transl conf "(thousand separator)" in
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
        (transl_nth conf "branch/branches" (if n = 1 then 0 else 1));
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
            (commd conf ^^^ "m=RL&" ^<^ acces conf base a ^^^ "&l1="
             ^<^ string_of_int x1 ^<^ "&"
             ^<^ acces_n conf base (Adef.escaped "1") dp1
             ^^^ "&l2=" ^<^ string_of_int x2 ^<^ "&"
             ^<^ acces_n conf base (Adef.escaped "2") dp2
             ^^^ (if pp1 = None then Adef.escaped ""
                  else "&" ^<^ acces_n conf base (Adef.escaped "3") p1)
             ^^^ (if pp2 = None then Adef.escaped ""
                  else "&" ^<^ acces_n conf base (Adef.escaped "4") p2)
             ^^^ (if propose_dag then Adef.escaped "&dag=on"
                  else Adef.escaped "")
             ^>^ if img then "" else "&im=0"
              :> string)
            (transl conf "see" |> Utf8.capitalize_fst)
            (if n > 1 && not propose_dag then transl conf " the first branch"
             else "")
        in
        Output.print_sstring conf str)
    list;
  Output.print_sstring conf "</ul>"

let print_solution_not_ancestor conf base long p1 p2 sol =
  let img = Util.get_opt conf "im" true in
  let pp1, pp2, (x1, x2, list), reltab = sol in
  Output.print_sstring conf {|<ul class="li_relationship"><li>|};
  transl conf "indeed," |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf " <ul>";
  List.iter
    (fun (a, n) ->
      Output.print_sstring conf "<li>\n";
      Output.print_string conf (person_title_text conf base a);
      Output.print_sstring conf "\n<em>(";
      Output.print_sstring conf (string_of_int n);
      Output.print_sstring conf " ";
      (if n = 1 then 0 else 1)
      |> transl_nth conf "relationship link/relationship links"
      |> Output.print_sstring conf;
      Output.print_sstring conf ")</em> &nbsp;";
      if not long then (
        let propose_dag = n > 1 && n <= 10 in
        let dp1 = match pp1 with Some p -> p | _ -> p1 in
        let dp2 = match pp2 with Some p -> p | _ -> p2 in
        Output.print_sstring conf {|<img src="|};
        Output.print_sstring conf (Util.images_prefix conf);
        Output.print_sstring conf {|/picto_rel_small.png" alt="">|};
        let href =
          commd conf ^^^ "m=RL&" ^<^ acces conf base a ^^^ "&l1="
          ^<^ string_of_int x1 ^<^ "&"
          ^<^ acces_n conf base (Adef.escaped "1") dp1
          ^^^ "&l2=" ^<^ string_of_int x2 ^<^ "&"
          ^<^ acces_n conf base (Adef.escaped "2") dp2
          ^^^ (if pp1 = None then Adef.escaped ""
               else "&" ^<^ acces_n conf base (Adef.escaped "3") p1)
          ^^^ (if pp2 = None then Adef.escaped ""
               else "&" ^<^ acces_n conf base (Adef.escaped "4") p2)
          ^^^ (if propose_dag then Adef.escaped "&dag=on" else Adef.escaped "")
          ^>^ if img then "" else "&im=0"
        in
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf href;
        Output.print_sstring conf {|">|};
        transl conf "see" |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf "</a>");
      Output.print_sstring conf "</li>")
    list;
  Output.print_sstring conf "</ul>";
  let is_are =
    match list with [ _ ] -> transl conf "is" | _ -> transl conf "are"
  in
  Output.print_sstring conf is_are;
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "at the same time");
  Output.print_sstring conf " ";
  let lab proj x =
    let info = (((reltab, list), x), proj) in
    match list with
    | [ (a, _) ] -> ancestor_label conf base info x (Driver.get_sex a)
    | _ -> parents_label conf base info x
  in
  let print pp p (alab : Adef.safe_string) =
    let s = gen_person_title_text no_reference conf base p in
    let s =
      match pp with
      | None -> transl_a_of_b conf (alab :> string) (s :> string) (s :> string)
      | Some pp ->
          transl_a_of_gr_eq_gen_lev conf
            (let s =
               transl_nth conf "the spouse" (index_of_sex (Driver.get_sex pp))
             in
             transl_a_of_b conf (alab :> string) s s)
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
  Output.print_sstring conf {|<div>&#9654;&nbsp;|};
  print_link_name conf base n p1 p2 sol;
  Output.print_sstring conf "</div>\n";
  if x1 = 0 || x2 = 0 then
    print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list
  else print_solution_not_ancestor conf base long p1 p2 sol;
  Output.print_sstring conf "\n"

let max_br = 33

let print_dag_links conf base p1 p2 rl =
  let module O = struct
    type t = Driver.iper

    let compare = compare
  end in
  let module M = Map.Make (O) in
  let sps = Util.get_opt conf "sp" true in
  let img = Util.get_opt conf "im" true in
  let anc_map =
    List.fold_left
      (fun anc_map (pp1, pp2, (x1, x2, list), _) ->
        List.fold_left
          (fun anc_map (p, n) ->
            let pp1, pp2, nn, nt, maxlev =
              try M.find (Driver.get_iper p) anc_map
              with Not_found -> (pp1, pp2, 0, 0, 0)
            in
            if nn >= max_br then anc_map
            else
              let v = (pp1, pp2, nn + n, nt + 1, max maxlev (max x1 x2)) in
              M.add (Driver.get_iper p) v anc_map)
          anc_map list)
      M.empty rl
  in
  let is_anc =
    match rl with (_, _, (x1, x2, _), _) :: _ -> x1 = 0 || x2 = 0 | _ -> false
  in
  let something =
    M.fold
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
    M.iter
      (fun ip (pp1, pp2, nn, nt, _) ->
        let dp1 = match pp1 with Some p -> p | _ -> p1 in
        let dp2 = match pp2 with Some p -> p | _ -> p2 in
        if nt > 1 && nn > 1 && nn < max_br then (
          let a = pget conf base ip in
          if not is_anc then (
            Output.print_sstring conf "<li>";
            Output.print_string conf (person_title_text conf base a);
            Output.print_sstring conf (Util.transl conf ":");
            Output.print_sstring conf " ");
          Output.print_sstring conf "<a href=\"";
          Output.print_string conf (commd conf);
          Output.print_sstring conf "m=RL&";
          Output.print_string conf (acces conf base a);
          Output.print_sstring conf "&";
          Output.print_string conf (acces_n conf base (Adef.escaped "1") dp1);
          Output.print_sstring conf "&";
          Output.print_string conf (acces_n conf base (Adef.escaped "2") dp2);
          if pp1 <> None then (
            Output.print_sstring conf "&";
            Output.print_string conf (acces_n conf base (Adef.escaped "3") p1));
          if pp2 <> None then (
            Output.print_sstring conf "&";
            Output.print_string conf (acces_n conf base (Adef.escaped "4") p2));
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
          if p_getenv conf.env "bd" = Some "on" then
            Output.print_sstring conf "&bd=on";
          Output.print_sstring conf {|&dag=on">|};
          if is_anc then Output.print_sstring conf (transl conf "tree")
          else (
            Output.print_sstring conf (string_of_int nn);
            Output.print_sstring conf " ";
            Output.print_sstring conf
              (transl_nth conf "relationship link/relationship links" 1));
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
      let s = (person_title_text conf base p : Adef.safe_string :> string) in
      let str =
        Printf.sprintf
          {|
          <div>
            <img src="%s/picto_rel_asc.png" alt="">
            <a href="%s%s&m=A&t=X&%s&l=%s">%s %s</a>.
          </div>
        |}
          (Util.images_prefix conf :> string)
          (commd conf :> string)
          (acces conf base p :> string)
          (acces_n conf base (Adef.escaped "1") a :> string)
          (string_of_int maxlen)
          (transl_a_of_b conf (transl_nth conf "ancestor/ancestors" 1) s s
          |> translate_eval |> Utf8.capitalize_fst)
          ((person_title_text conf base a : Adef.safe_string :> string)
          |> transl_decline conf "up to")
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
      let bd = Option.value ~default:0 (p_getint conf.env "bd") in
      let td_prop =
        match Util.p_getenv conf.env "color" with
        | None | Some "" -> Adef.safe ""
        | Some x ->
            (" class=\"" ^<^ Mutil.encode x ^>^ "\"" :> Adef.safe_string)
      in
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
            bd;
            td_prop;
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

type 'a env = Vother of 'a | Vnone

let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

let print_main_relationship conf base long p1 p2 rel =
  let total =
    match rel with None -> Sosa.zero | Some (_, total, _) -> total
  in
  let title _ =
    transl conf "relationship" |> Utf8.capitalize_fst
    |> Output.print_sstring conf;
    if not (Sosa.eq total Sosa.zero) then (
      Output.print_sstring conf " (";
      Output.print_sstring conf
        (Sosa.to_string_sep (transl conf "(thousand separator)") total);
      Output.print_sstring conf " ";
      (if Sosa.eq total Sosa.one then 0 else 1)
      |> transl_nth conf "relationship link/relationship links"
      |> Output.print_sstring conf;
      Output.print_sstring conf ")")
  in
  Hutil.header conf title;
  Output.print_sstring conf
    "\n<!-- Generated by RelationDisplay.print_main_relationship -->\n";
  (match p_getenv conf.env "cgl" with
  | Some "on" -> ()
  | _ ->
      let conf = { conf with is_printed_by_template = false } in
      Templ.output_simple conf Templ.Env.empty "buttons_rel");
  (match (Util.p_getenv conf.env "sp", Util.p_getenv conf.env "spouse") with
  | Some ("off" | "0"), _ | _, Some "off" ->
      conf.senv <- conf.senv @ [ ("sp", Mutil.encode "0") ]
  | _, _ -> ());
  (match p_getenv conf.env "cgl" with
  | Some "on" -> conf.senv <- conf.senv @ [ ("cgl", Mutil.encode "on") ]
  | _ -> ());
  (match p_getenv conf.env "bd" with
  | None | Some ("0" | "") -> ()
  | Some x -> conf.senv <- conf.senv @ [ ("bd", Mutil.encode x) ]);
  (match p_getenv conf.env "color" with
  | None | Some "" -> ()
  | Some x -> conf.senv <- conf.senv @ [ ("color", Mutil.encode x) ]);
  (match rel with
  | None ->
      if Driver.get_iper p1 = Driver.get_iper p2 then (
        transl conf "it is the same person!"
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf " ")
      else (
        ([
           gen_person_title_text reference conf base p1;
           gen_person_title_text reference conf base p2;
         ]
          : Adef.safe_string list
          :> string list)
        |> cftransl conf "no known relationship link between %s and %s"
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf {|.<br><p><span><a href="|};
        Output.print_string conf (commd conf);
        Output.print_sstring conf "&m=R&";
        Output.print_string conf (acces conf base p1);
        Output.print_sstring conf {|">|};
        transl_nth conf "try another/relationship computing" 0
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf "</a> ";
        Output.print_sstring conf
          (transl_nth conf "try another/relationship computing" 1);
        Output.print_sstring conf ".</span></p>")
  | Some (rl, _, relationship) ->
      let a1 = p1 in
      let a2 = p2 in
      let all_by_marr =
        List.for_all
          (function Some _, _, _, _ | _, Some _, _, _ -> true | _ -> false)
          rl
      in
      let _ =
        List.fold_left
          (fun i sol ->
            print_solution conf base long i p1 p2 sol;
            if long then print_path conf base p1 p2 sol;
            succ i)
          1 rl
      in
      Output.print_sstring conf "\n";
      if long then () else print_dag_links conf base p1 p2 rl;
      if
        (not all_by_marr)
        && authorized_age conf base p1
        && authorized_age conf base p2
        && Driver.get_consang a1 != Adef.fix (-1)
        && Driver.get_consang a2 != Adef.fix (-1)
      then (
        Output.print_sstring conf "<p>\n";
        Output.printf conf "<em>%s%s %s%%</em>"
          (Utf8.capitalize_fst (transl conf "relationship"))
          (Util.transl conf ":")
          (string_of_decimal_num conf
             (round_2_dec
                (Adef.float_of_fix (Adef.fix_of_float relationship) *. 100.0)));
        Output.print_sstring conf "</p>\n");
      print_propose_upto conf base p1 p2 rl);
  Hutil.trailer conf

let multi_relation_next_txt conf pl2 lim assoc_txt =
  let assoc_txt : (Geneweb_db.Driver.iper, string) Hashtbl.t = assoc_txt in
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
              try
                "&t" ^<^ string_of_int n ^<^ "="
                ^<^ (Driver.get_iper p |> Hashtbl.find assoc_txt |> Mutil.encode
                      :> Adef.escaped_string)
                ^^^ acc
              with Not_found -> acc
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
      commd conf ^^^ "m=RLM" ^<^ acc

let print_no_relationship conf base pl =
  let title _ =
    transl conf "tree" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<ul>";
  List.iter
    (fun p ->
      Output.print_sstring conf "<li>";
      Output.print_string conf (referenced_person_title_text conf base p);
      Output.print_sstring conf "</li>")
    pl;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_multi_relation conf base pl lim assoc_txt =
  let assoc_txt : (Geneweb_db.Driver.iper, string) Hashtbl.t = assoc_txt in
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
  let path =
    let rec loop path = function
      | p1 :: (p2 :: _ as pl) -> (
          let ip1 = Driver.get_iper p1 in
          let ip2 = Driver.get_iper p2 in
          match get_shortest_path_relation conf base ip1 ip2 [] with
          | Some (path1, _) ->
              let path =
                match path with
                | [] -> path1
                | _ -> (
                    match List.rev path1 with
                    | _ :: path1 -> List.rev path1 @ path
                    | [] -> path)
              in
              loop path pl
          | None -> loop path pl)
      | [ _ ] | [] -> path
    in
    loop [] pl1
  in
  if path = [] then print_no_relationship conf base pl
  else
    let elem_txt p =
      let content =
        try
          let txt = Hashtbl.find assoc_txt (Driver.get_iper p) in
          if txt <> "" then
            "<b>(" ^<^ (Util.escape_html txt :> Adef.safe_string) ^>^ ")</b>"
          else Adef.safe ""
        with Not_found -> Adef.safe ""
      in
      DagDisplay.Item (p, content)
    in
    let vbar_txt _ = Adef.escaped "" in
    let next_txt = multi_relation_next_txt conf pl2 lim assoc_txt in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print_base_loop conf base p =
  let title _ =
    transl conf "error" |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Output.printf conf
    (fcapitale (ftransl conf "loop in database: %s is his/her own ancestor"))
    (Util.update_family_loop conf base p
       (Util.designation base p : Adef.escaped_string :> Adef.safe_string)
      :> string);
  Output.print_sstring conf ".";
  Hutil.trailer conf

let relmenu_print = Perso.interp_templ "relmenu"

let print conf base p = function
  | Some p1 -> (
      match p_getenv conf.env "et" with
      | Some "S" -> print_shortest_path conf base p1 p
      | x -> (
          let by_marr = x = Some "M" in
          let long =
            match p_getenv conf.env "long" with Some "on" -> true | _ -> false
          in
          match
            try Left (compute_relationship conf base by_marr p1 p)
            with Consang.TopologicalSortError p -> Right p
          with
          | Left rel -> print_main_relationship conf base long p1 p rel
          | Right p -> print_base_loop conf base p))
  | None -> relmenu_print conf base p

let print_multi conf base =
  let assoc_txt : (Geneweb_db.Driver.iper, string) Hashtbl.t =
    Hashtbl.create 53
  in
  let pl =
    let rec loop pl i =
      let k = string_of_int i in
      match find_person_in_env conf base k with
      | Some p ->
          (match p_getenv conf.env ("t" ^ k) with
          | Some x -> Hashtbl.add assoc_txt (Driver.get_iper p) x
          | None -> ());
          loop (p :: pl) (i + 1)
      | None -> List.rev pl
    in
    loop [] 1
  in
  let lim = Option.value ~default:0 (p_getint conf.env "lim") in
  print_multi_relation conf base pl lim assoc_txt
