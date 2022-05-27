(* Copyright (c) 1998-2007 INRIA *)

open Geneweb.Config
open Def
open Geneweb.TemplAst
open Gwdb
open Geneweb.Util
open Geneweb.DescendDisplay
open Geneweb.Relation
open Geneweb.RelationDisplay
open Geneweb.RelationLink
open Geneweb.Dag2html

module Dag = Geneweb.Dag
module Dag2html = Geneweb.Dag2html
module DagDisplay = Geneweb.DagDisplay
module DateDisplay = Geneweb.DateDisplay
module RelationDisplay = Geneweb.RelationDisplay
module RelationLink = Geneweb.RelationLink
module Util = Geneweb.Util
module Hutil = Geneweb.Hutil
module Output = Geneweb.Output

let dag_of_ind_dag_list indl =
  let (indl, _) =
    List.fold_right (fun ind (indl, cnt) -> (ind, cnt) :: indl, cnt + 1) indl
      ([], 0)
  in
  let idag_of_di_ind ind = idag_of_int (List.assq ind indl) in
  List.map
    (fun (ind, cnt) ->
       {pare = List.map idag_of_di_ind ind.di_famc.df_pare;
        valu =
          begin match ind.di_val with
            Some ic -> Def.Left ic
          | None -> Def.Right cnt
          end;
        chil = List.map idag_of_di_ind ind.di_fams.df_chil})
    indl

let dag_of_relation_path conf base path =
  let indl = dag_ind_list_of_path path in
  let indl = add_missing_parents_of_siblings conf base indl in
  let faml = dag_fam_list_of_ind_list indl in
  let indl = add_phony_children indl faml in
  let nl = dag_of_ind_dag_list indl in
  let d = {dag = Array.of_list (List.rev nl)} in
  let set =
    List.fold_left
      (fun set n ->
         match n.valu with
           Def.Left ip -> Dag.Pset.add ip set
         | Def.Right _ -> set)
      Dag.Pset.empty nl
  in
  set, d

let old_print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  let invert =
    match p_getenv conf.env "invert" with
      Some "on" -> true
    | _ -> false
  in
  let (set, d) = dag_of_relation_path conf base path in
  let page_title = Utf8.capitalize_fst (transl conf "relationship") in
  let hts = DagDisplay.make_tree_hts conf base elem_txt vbar_txt invert set [] d in
  DagDisplay.print_slices_menu_or_dag_page conf base page_title hts next_txt

let print_relationship_dag conf base elem_txt vbar_txt path next_txt =
  if p_getenv conf.env "new" <> Some "on" then
    old_print_relationship_dag conf base elem_txt vbar_txt path next_txt
  else
    let invert =
      match p_getenv conf.env "invert" with
        Some "on" -> true
      | _ -> false
    in
    let set = ind_set_of_relation_path base path in
    let page_title = Utf8.capitalize_fst (transl conf "relationship") in
    DagDisplay.make_and_print_dag conf base elem_txt vbar_txt invert set []
      page_title next_txt

let print_relation_path conf base ip1 ip2 path ifam excl_faml =
  if path = [] then
    let title _ =
      Output.print_string conf (Utf8.capitalize_fst (transl conf "relationship"))
    in
    Hutil.header_no_page_title conf title; Hutil.trailer conf
  else
    let next_txt = RelationDisplay.next_relation_link_txt conf ip1 ip2 (ifam :: excl_faml) in
    let elem_txt p = DagDisplay.Item (p, "") in
    let vbar_txt ip =
      let u = pget conf base ip in
      let excl_faml = Array.to_list (get_family u) @ excl_faml in
      RelationDisplay.next_relation_link_txt conf ip1 ip2 excl_faml
    in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print_shortest_path conf base p1 p2 =
  let ip1 = get_iper p1 in
  let ip2 = get_iper p2 in
  if ip1 = ip2 then
    let title _ =
      Output.print_string conf (Utf8.capitalize_fst (transl conf "relationship"))
    in
    Hutil.header conf title;
    Output.print_string conf (Utf8.capitalize_fst (transl conf "it is the same person!"));
    Output.print_string conf "\n";
    Hutil.trailer conf
  else
    let excl_faml = excl_faml conf base in
    let title _ =
      Output.print_string conf (Utf8.capitalize_fst (transl conf "relationship"))
    in
    match get_shortest_path_relation conf base ip1 ip2 excl_faml with
    | Some (path, ifam) ->
        print_relation_path conf base ip1 ip2 path ifam excl_faml
    | None ->
        let s1 = gen_person_title_text reference raw_access conf base p1 in
        let s2 = gen_person_title_text reference raw_access conf base p2 in
        Hutil.header_no_page_title conf title;
        if excl_faml = [] then
          begin
            Output.print_string conf "<h1>";
            title false;
            Output.print_string conf "</h1>\n";
            Hutil.print_link_to_welcome conf true;
            Output.printf conf "%s.\n"
              (Utf8.capitalize_fst
                 (cftransl conf "no known relationship link between %s and %s"
                    [s1; s2]));
            Output.print_string conf "<br>\n";
            begin
              Output.print_string conf "<p>\n";
              begin
                Output.print_string conf "<span>";
                begin
                  Output.printf conf "<a href=\"%s&m=R&%s\">" (commd conf)
                    (acces conf base p1);
                  Output.print_string conf
                    (Utf8.capitalize_fst
                       (transl_nth conf "try another/relationship computing"
                          0));
                  Output.print_string conf "</a>"
                end;
                Output.printf conf " %s.\n"
                  (transl_nth conf "try another/relationship computing" 1);
                Output.print_string conf "</span>"
              end;
              Output.print_string conf "</p>\n"
            end
          end
        else
          begin
            let title _ =
              Output.print_string conf
                ((Utf8.capitalize_fst (transl_nth conf "relation/relations" 0)) ^
                " (" ^ (transl conf "shortest path") ^ ")")
            in
            Output.print_string conf "<h1>";
            title false;
            Output.print_string conf "</h1>\n";
            Hutil.print_link_to_welcome conf true;
            Util.include_template conf conf.env "buttons_rel" (fun () -> ());
            Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.command;
            Output.print_string conf
            ((Utf8.capitalize_fst (transl conf "no more")) ^ " " ^
              (transl conf "link between"));
            Output.printf conf " %s " s1;
            Output.print_string conf (transl conf "and");
            Output.printf conf " %s.\n" s2;
            Output.print_string conf "</form>\n";
          end;
        Hutil.trailer conf

let print_main_relationship conf base long p1 p2 rel =
  let total =
    match rel with
      None -> Sosa.zero
    | Some (_, total, _) -> total
  in
  let title _ =
    match p_getenv conf.env "et" with
    | Some "A" ->
        Output.print_string conf (Utf8.capitalize_fst (transl conf "relationship by ancestors"))
    | Some "M" ->
        Output.print_string conf (Utf8.capitalize_fst (transl conf "relationship by marriage"))
    | Some "S" ->
        Output.print_string conf (Utf8.capitalize_fst (transl_nth conf "relation/relations" 0) ^
          " (" ^ (transl conf "shortest path") ^ ")")
    | _ -> 
        Output.print_string conf (Utf8.capitalize_fst (transl conf "relationship"));
    if Sosa.eq total Sosa.zero then ()
    else
      begin
        Output.printf conf " (%s %s)"
          (Sosa.to_string_sep (transl conf "(thousand separator)") total)
          (transl_nth conf "relationship link/relationship links"
             (if Sosa.eq total Sosa.one then 0 else 1))
      end
  in
  let conf =
    if long then
      let doctype =
        match p_getenv conf.base_env "doctype" with
          Some ("html-4.01" | "html-4.01-trans") -> "html-4.01-trans"
        | _ -> "xhtml-1.0-trans"
      in
      {conf with base_env = ("doctype", doctype) :: conf.base_env}
    else conf
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Util.include_template conf conf.env "buttons_rel" (fun () -> ());
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.command;
  begin match p_getenv conf.env "spouse" with
    Some "on" -> conf.senv <- conf.senv @ ["spouse", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "cgl" with
    Some "on" -> conf.senv <- conf.senv @ ["cgl", "on"]
  | _ -> ()
  end;
  begin match p_getenv conf.env "bd" with
    None | Some ("0" | "") -> ()
  | Some x -> conf.senv <- conf.senv @ ["bd", x]
  end;
  begin match p_getenv conf.env "color" with
    None | Some "" -> ()
  | Some x -> conf.senv <- conf.senv @ ["color", Mutil.encode x]
  end;
  begin match rel with
    None ->
      if get_iper p1 = get_iper p2 then
        Output.printf conf "%s\n"
          (Utf8.capitalize_fst (transl conf "it is the same person!"))
      else
        begin
          Output.printf conf "%s.\n"
            (Utf8.capitalize_fst
               (cftransl conf "no known relationship link between %s and %s"
                  [gen_person_title_text reference raw_access conf base p1;
                   gen_person_title_text reference raw_access conf base p2]));
          Output.print_string conf "<br>\n";
          begin
            Output.print_string conf "<p>\n";
            begin
              Output.print_string conf "<span>";
              begin
                Output.printf conf "<a href=\"%s&m=R&%s\">" (commd conf)
                  (acces conf base p1);
                Output.print_string conf
                  (Utf8.capitalize_fst
                     (transl_nth conf "try another/relationship computing"
                        0));
                Output.print_string conf "</a>"
              end;
              Output.printf conf " %s.\n"
                (transl_nth conf "try another/relationship computing" 1);
              Output.print_string conf "</span>"
            end;
            Output.print_string conf "</p>\n"
          end
        end
  | Some (rl, _, relationship) ->
      let a1 = p1 in
      let a2 = p2 in
      let all_by_marr =
        List.for_all
          (function
             Some _, _, _, _ | _, Some _, _, _ -> true
           | _ -> false)
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
      Output.print_string conf "\n";
      if long then () else print_dag_links conf base p1 p2 rl;
      if not all_by_marr && authorized_age conf base p1 &&
         authorized_age conf base p2 && get_consang a1 != Adef.fix (-1) &&
         get_consang a2 != Adef.fix (-1)
      then
        begin
          Output.print_string conf "<p>\n";
          Output.printf conf "<em>%s%s %s%%</em>"
            (Utf8.capitalize_fst (transl conf "relationship"))
            (Util.transl conf ":")
            (string_of_decimal_num conf
               (round_2_dec
                  (Adef.float_of_fix (Adef.fix_of_float relationship) *.
                   100.0)));
          Output.print_string conf "</p>\n"
        end;
      print_propose_upto conf base p1 p2 rl
  end;
  Output.print_string conf "</form>\n";
  Hutil.trailer conf

let print_multi_relation conf base pl lim assoc_txt =
  let (pl1, pl2) =
    if lim <= 0 then pl, []
    else
      let rec loop n rev_pl1 pl2 =
        match n, pl2 with
          _, [] | _, [_] -> pl, []
        | 0, _ ->
            begin match rev_pl1 with
              p :: _ -> List.rev rev_pl1, p :: pl2
            | _ -> pl, []
            end
        | n, p :: pl -> loop (n - 1) (p :: rev_pl1) pl
      in
      loop lim [] pl
  in
  let path =
    let rec loop path =
      function
        p1 :: (p2 :: _ as pl) ->
          let ip1 = get_iper p1 in
          let ip2 = get_iper p2 in
          begin match get_shortest_path_relation conf base ip1 ip2 [] with
            Some (path1, _) ->
              let path =
                match path with
                  [] -> path1
                | _ ->
                    match List.rev path1 with
                      _ :: path1 -> List.rev path1 @ path
                    | [] -> path
              in
              loop path pl
          | None -> loop path pl
          end
      | [_] | [] -> path
    in
    loop [] pl1
  in
  if path = [] then RelationDisplay.print_no_relationship conf base pl
  else
    let elem_txt p =
      DagDisplay.Item
        (p,
         (try
            let t = Hashtbl.find assoc_txt (get_iper p) in
            "<b>(" ^ t ^ ")</b>"
          with Not_found -> ""))
    in
    let vbar_txt _ = "" in
    let next_txt = RelationDisplay.multi_relation_next_txt conf pl2 lim assoc_txt in
    print_relationship_dag conf base elem_txt vbar_txt path next_txt

let print conf base p =
  function
    Some p1 ->
      begin match p_getenv conf.env "et" with
        Some "S" -> print_shortest_path conf base p1 p
      | x ->
          let by_marr = x = Some "M" in
          let long =
            match p_getenv conf.env "long" with
              Some "on" -> true
            | _ -> false
          in
          match
            try Def.Left (compute_relationship conf base by_marr p1 p) with
              Consang.TopologicalSortError p -> Right p
          with
            Left rel -> print_main_relationship conf base long p1 p rel
          | Right p -> RelationDisplay.print_base_loop conf base p
      end
  | None ->   Util.include_template conf conf.env "relmenu" (fun () -> ())


let print_multi conf base =
  let assoc_txt = Hashtbl.create 53 in
  let pl =
    let rec loop pl i =
      let k = string_of_int i in
      match find_person_in_env conf base k with
        Some p ->
          begin match p_getenv conf.env ("t" ^ k) with
            Some x -> Hashtbl.add assoc_txt (get_iper p) x
          | None -> ()
          end;
          loop (p :: pl) (i + 1)
      | None -> List.rev pl
    in
    loop [] 1
  in
  let lim =
    match p_getint conf.env "lim" with
      Some x -> x
    | None -> 0
  in
  print_multi_relation conf base pl lim assoc_txt
