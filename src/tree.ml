(* camlp4r ./pa_html.cmo *)
(* $Id: tree.ml,v 2.5 1999-08-22 15:09:41 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

type t 'a = { node : 'a; sons : list (t 'a) };

value rec tree_of_branch =
  fun 
  [ [] -> invalid_arg "tree_of_branch"
  | [x] -> {node = x; sons = []}
  | [x :: l] -> {node = x; sons = [tree_of_branch l]} ]
;

value rec insert l tl =
  match (l, tl) with
  [ ([x :: l], [t :: tl]) ->
      if x == t.node then
        let t = {node = t.node; sons = insert l t.sons} in
        [t :: tl]
      else [t :: insert [x :: l] tl]
  | (l, []) -> [tree_of_branch l]
  | ([], _) -> tl ]
;

value append t =
  fun
  [ [] -> t
  | [x :: l] ->
      let rec app t =
        if x == t.node then Some {node = t.node; sons = insert l t.sons}
        else
          match app_rev_sons (List.rev t.sons) with
          [ Some rev_tl -> Some {node = t.node; sons = List.rev rev_tl}
          | None -> None ]
      and app_rev_sons =
        fun
        [ [last :: rev_tl] ->
            match app last with
            [ Some t -> Some [t :: rev_tl]
            | None ->
                match app_rev_sons rev_tl with
                [ Some rev_tl -> Some [last :: rev_tl]
                | None -> None ] ]
        | [] -> None ]
      in
      match app t with
      [ Some t -> t
      | None -> t ] ]
;

value colspan_tree =
  loop 1 where rec loop n1 t =
    let (rev_tree, n2) =
      List.fold_left
        (fun (tree, n) t ->
           let (t, n) = loop n t in
           ([t :: tree], n))
        ([], n1) t.sons
    in
    let n2 = if rev_tree = [] then n2 + 3 else n2 in
    ({node = (t.node, n1, n2); sons = List.rev rev_tree}, n2)
;

value print_tree conf base with_spouses one_branch t =
  let (_, _, last_n) = t.node in
  let print_person cs ip parent t =
    do stag "td" begin Wserver.wprint "&nbsp;"; end;
       Wserver.wprint "\n";
       let p = poi base ip in
       stag "td" "colspan=%d align=center%s" (cs - 2)
         (if with_spouses then
            if t.sons = [] then " valign=top" else " valign=bottom"
          else "")
       begin
         Wserver.wprint "%s" (referenced_person_title_text conf base p);
         Wserver.wprint "%s" (Date.short_dates_text conf base p);
       end;
       Wserver.wprint "\n";
       stag "td" begin Wserver.wprint "&nbsp;"; end;
    return ()
  in
  let print_spouse cs ip parent t =
    do stag "td" begin Wserver.wprint "&nbsp;"; end;
       Wserver.wprint "\n";
       let p = poi base ip in
       stag "td" "colspan=%d align=center valign=top" (cs - 2) begin
         match t.sons with
         [ [{node = (ip, _, _)} :: _] ->
             match (aoi base ip).parents with
             [ Some ifam ->
                 let sp = poi base (spouse p (coi base ifam)) in
                 do Wserver.wprint "\n&amp;%s\n"
                      (Date.short_marriage_date_text conf base (foi base ifam)
                         p sp);
                    Wserver.wprint "%s"
                      (referenced_person_title_text conf base sp);
                    Wserver.wprint "%s" (Date.short_dates_text conf base sp);
                 return ()
             | _ -> Wserver.wprint "&nbsp;" ]
         | _ -> Wserver.wprint "&nbsp;" ];
       end;
       Wserver.wprint "\n";
       stag "td" begin Wserver.wprint "&nbsp;"; end;
    return ()
  in
  let print_vertical_bar cs ip parent t =
    stag "td" "colspan=%d align=center" cs begin
      Wserver.wprint "|";
    end
  in
  let print_vertical_bar_if_sons cs ip parent t =
    stag "td" "colspan=%d align=center" cs begin
      if t.sons <> [] then Wserver.wprint "|" else Wserver.wprint "&nbsp;";
    end
  in
  let print_horizontal_bar cs ip (_, pn1, pn2) t =
    let (_, n1, n2) = t.node in
    let align =
      if n1 = pn1 && n2 = pn2 then " align=center"
      else if n1 = pn1 then " align=right"
      else if n2 = pn2 then " align=left"
      else ""
    in
    stag "td" "colspan=%d%s" cs align begin
      if n1 = pn1 && n2 = pn2 then Wserver.wprint "|"
      else if n1 = pn1 || n2 = pn2 then
        Wserver.wprint "<hr noshade size=1 width=\"50%%\"%s>" align
      else Wserver.wprint "<hr noshade size=1>";
    end
  in
  let print_row print_data tl =
    tag "tr" begin
      let n2 =
        List.fold_left
          (fun n0 (parent, t) ->
             let (ip, n1, n2) = t.node in
             do if n1 > n0 then
                  do stag "td" "colspan=%d" (n1 - n0) begin
                       Wserver.wprint "&nbsp;";
                     end;
                     Wserver.wprint "\n";
                  return ()
                else ();
                print_data (n2 - n1) ip parent t;
                Wserver.wprint "\n";
             return n2)
          1 tl
      in
      if last_n > n2 then
        do stag "td" "colspan=%d" (last_n - n2) begin
             Wserver.wprint "&nbsp;";
           end;
           Wserver.wprint "\n";
        return ()
      else ();
    end
  in
  let border =
    match p_getenv conf.env "border" with
    [ Some x -> x
    | None -> "0" ]
  in
  tag "table" "border=%s cellspacing=0 cellpadding=0 width=\"100%%\"" border
  begin
    loop True [(t.node, t)] where rec loop first tl =
      do if not first && not one_branch then
           do print_row print_horizontal_bar tl;
              print_row print_vertical_bar tl;
           return ()
         else ();
         print_row print_person tl;
         let last = List.for_all (fun (_, t) -> t.sons = []) tl in
         if not last then
           do if with_spouses then print_row print_spouse tl else ();
              print_row print_vertical_bar_if_sons tl;
           return ()
         else ();
      return
      let tl =
        List.fold_right
          (fun (_, t) tl -> List.map (fun n -> (t.node, n)) t.sons @ tl) tl []
      in
      if tl = [] then () else loop False tl;
  end
;

value print_branch_list_as_tree conf base with_spouses =
  fun
  [ [] -> ()
  | [b :: bl] ->
      let t = List.fold_left append (tree_of_branch b) bl in
      let one_branch =
        loop t.sons where rec loop =
          fun
          [ [] -> True
          | [t] -> loop t.sons
          | _ -> False ]
      in
      let (t, _) = colspan_tree t in
      print_tree conf base with_spouses one_branch t ]
;

value find_branch_list conf base =
  loop 1 where rec loop i =
    let s = string_of_int i in
    let po = find_person_in_env conf base s in
    let so = p_getenv conf.env ("s" ^ s) in
    match (po, so) with
    [ (Some p, Some s) ->
        match branch_of_sosa base p.cle_index (Num.of_string s) with
        [ Some ipsl -> [List.map fst ipsl :: loop (i + 1)]
        | None -> loop (i + 1) ]
    | _ -> [] ]
;

value print_branches conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "tree")) in
  let bl = find_branch_list conf base in
  let with_spouses =
    match p_getenv conf.env "spouse" with
    [ Some "on" -> True
    | _ -> False ]
  in
  do header_no_page_title conf title;
     print_branch_list_as_tree conf base with_spouses bl;
     trailer conf;
  return ()
;

value print conf base =
  print_branches conf base
;
