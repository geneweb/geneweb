(* camlp4r ./pa_html.cmo *)
(* $Id: relationLink.ml,v 1.17 1999-02-22 04:40:46 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

(* Algorithm *)

type dist =
  { dmin : mutable int;
    dmax : mutable int;
    mark : bool }
;

value infinity = 1000;

value threshold = ref 15;

value phony_dist_tab = (fun _ -> 0, fun _ -> infinity);

value make_dist_tab conf base ia maxlev =
  if maxlev <= threshold.val then phony_dist_tab
  else
    let _ = base.data.ascends.array () in
    let _ = base.data.couples.array () in
    let tstab = Util.create_topological_sort conf base in
    let module Pq =
      Pqueue.Make
        (struct
           type t = int;
           value leq x y = not (Consang.tsort_leq tstab x y);
         end)
    in
    let default = {dmin = infinity; dmax = 0; mark = False} in
    let dist = Array.create base.data.persons.len default in
    let q = ref Pq.empty in
    let add_children ip =
      let p = poi base ip in
      for i = 0 to Array.length p.family - 1 do
        let fam = foi base p.family.(i) in
        for j = 0 to Array.length fam.children - 1 do
          let k = Adef.int_of_iper fam.children.(j) in
          let d = dist.(k) in
          if not d.mark then
            do dist.(k) := {dmin = infinity; dmax = 0; mark = True};
               q.val := Pq.add k q.val;
            return ()
          else ();
        done;
      done
    in
    do dist.(Adef.int_of_iper ia) := {dmin = 0; dmax = 0; mark = True};
       add_children ia;
       while not (Pq.is_empty q.val) do
         let (k, nq) = Pq.take q.val in
         do q.val := nq; return
         match (base.data.ascends.get k).parents with
         [ Some ifam ->
             let cpl = coi base ifam in
             let dfath = dist.(Adef.int_of_iper cpl.father) in
             let dmoth = dist.(Adef.int_of_iper cpl.mother) in
             do dist.(k).dmin := min dfath.dmin dmoth.dmin + 1;
                dist.(k).dmax := max dfath.dmax dmoth.dmax + 1;
                if dist.(k).dmin > maxlev then ()
                else add_children (Adef.iper_of_int k);
             return ()
         | None -> () ];
       done; 
    return
    (fun ip -> dist.(Adef.int_of_iper ip).dmin,
     fun ip -> dist.(Adef.int_of_iper ip).dmax)
;

value find_first_branch base (dmin, dmax) ia =
  find [] where rec find br len ip sp =
    if ip == ia then if len == 0 then Some br else None
    else if len == 0 then None
    else
      if len < dmin ip || len > dmax ip then None
      else
        match (aoi base ip).parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            match find [(ip, sp) :: br] (len - 1) cpl.father Masculine with
            [ Some _ as r -> r
            | None -> find [(ip, sp) :: br] (len - 1) cpl.mother Feminine ]
        | None -> None ]
;

value rec next_branch_same_len base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    [ [] -> None
    | [(ip, sp) :: ipl1] ->
        match sa with
        [ Feminine ->
            next_branch_same_len base dist True (missing + 1) ip sp ipl1
        | Masculine ->
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                next_branch_same_len base dist False missing cpl.mother
                  Feminine ipl
            | _ -> failwith "next_branch_same_len" ]
        | Neuter -> assert False ] ]
  else if missing == 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    next_branch_same_len base dist True missing ia sa ipl
  else
    match (aoi base ia).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        next_branch_same_len base dist False (missing - 1) cpl.father Masculine
          [(ia, sa) :: ipl]
    | None -> next_branch_same_len base dist True missing ia sa ipl ]
;

value find_next_branch base dist ia sa ipl =
  loop ia sa ipl where rec loop ia1 sa1 ipl =
    match next_branch_same_len base dist True 0 ia1 sa1 ipl with
    [ Some (ia1, sa1, ipl) -> if ia == ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None ]
;

value rec prev_branch_same_len base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    [ [] -> None
    | [(ip, sp) :: ipl1] ->
        match sa with
        [ Masculine ->
            prev_branch_same_len base dist True (missing + 1) ip sp ipl1
        | Feminine ->
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                prev_branch_same_len base dist False missing cpl.father
                  Masculine ipl
            | _ -> failwith "prev_branch_same_len" ]
        | Neuter -> assert False ] ]
  else if missing == 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    prev_branch_same_len base dist True missing ia sa ipl
  else
    match (aoi base ia).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        prev_branch_same_len base dist False (missing - 1) cpl.mother Feminine
          [(ia, sa) :: ipl]
    | None -> prev_branch_same_len base dist True missing ia sa ipl ]
;

value find_prev_branch base dist ia sa ipl =
  loop ia sa ipl where rec loop ia1 sa1 ipl =
    match prev_branch_same_len base dist True 0 ia1 sa1 ipl with
    [ Some (ia1, sa1, ipl) -> if ia == ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None ]
;

(* Printing *)

value has_td_width_percent conf =
  let user_agent = Wserver.extract_param "user-agent: " '.' conf.request in
  String.lowercase user_agent <> "mozilla/1"
;

value print_someone conf base ip =
  let p = poi base ip in
  do afficher_personne_titre_referencee conf base p;
     Date.afficher_dates_courtes conf base p;
     Wserver.wprint "\n";
  return ()
;

value print_spouse conf base ip ipl =
  match (ipl, p_getenv conf.env "opt") with
  [ ([(ips, _) :: _], Some "spouse") ->
      let a = aoi base ips in
      match a.parents with
      [ Some ifam ->
          let c = coi base ifam in
          let sp =
            if ip = c.father then c.mother
            else c.father
          in
          do Wserver.wprint "&amp;";
             html_br conf;
             print_someone conf base sp;
          return ()
      | _ -> () ]
  | _ -> () ]
;

value rec print_both_branches conf base pl1 pl2 =
  if pl1 = [] && pl2 = [] then ()
  else
    let (p1, pl1) =
      match pl1 with
      [ [(p1, _) :: pl1] -> (Some p1, pl1)
      | [] -> (None, []) ]
    in
    let (p2, pl2) =
      match pl2 with
      [ [(p2, _) :: pl2] -> (Some p2, pl2)
      | [] -> (None, []) ]
    in
    do tag "tr" begin
         stag "td" "align=center" begin
           match p1 with
           [ Some p1 -> Wserver.wprint "|"
           | None -> () ];
         end;
         stag "td" "align=center" begin
           match p2 with
           [ Some p2 -> Wserver.wprint "|"
           | None -> () ];
         end;
         Wserver.wprint "\n";
       end;
       tag "tr" begin
         tag "td" "valign=top align=center%s"
           (if has_td_width_percent conf then " width=\"50%\"" else "")
         begin
           match p1 with
           [ Some p1 ->
               do print_someone conf base p1;
                  print_spouse conf base p1 pl1;
               return ()
           | None -> () ];
         end;
         tag "td" "valign=top align=center%s"
           (if has_td_width_percent conf then " width=\"50%\"" else "")
         begin
           match p2 with
           [ Some p2 ->
               do print_someone conf base p2;
                  print_spouse conf base p2 pl2;
               return ()
           | None -> () ];
         end;
       end;
    return print_both_branches conf base pl1 pl2
;

value rec print_one_branch conf base ipl1 =
  if ipl1 = [] then ()
  else
    let (ip1, ipl1) =
      match ipl1 with
      [ [(ip1, _) :: ipl1] -> (Some ip1, ipl1)
      | [] -> (None, []) ]
    in
    do match ip1 with
       [ Some ip1 -> do Wserver.wprint "|"; html_br conf; return ()
       | None -> () ];
       match ip1 with
       [ Some ip1 ->
           do print_someone conf base ip1;
              print_spouse conf base ip1 ipl1;
              html_br conf;
           return ()
       | None -> () ];
    return print_one_branch conf base ipl1
;

value print_sign conf sign ip sp i1 i2 b1 b2 c1 c2 =
  do Wserver.wprint "<a href=\"%sm=RL;i1=%d;i2=%d" (commd conf)
       (Adef.int_of_iper i1) (Adef.int_of_iper i2);
     Wserver.wprint ";b1=%s" (Num.to_string (sosa_of_branch [(ip, sp) :: b1]));
     Wserver.wprint ";b2=%s" (Num.to_string (sosa_of_branch [(ip, sp) :: b2]));
     Wserver.wprint ";c1=%d;c2=%d" c1 c2;
     Wserver.wprint "\">%s</a>" sign;
     Wserver.wprint "\n";
  return ()
;

value print_prev_next_1 conf base ip sp i1 i2 b1 b2 c1 c2 pb nb =
  do match pb with
     [ Some b1 ->
         let sign = "&lt;&lt;" in
         print_sign conf sign ip sp i1 i2 b1 b2 (c1 - 1) c2
     | _ -> () ];
     match (pb, nb) with
     [ (None, None) -> ()
     | _ -> Wserver.wprint "<font size=-1>%d</font>\n" c1 ];
     match nb with
     [ Some b1 ->
         let sign = "&gt;&gt;" in
         print_sign conf sign ip sp i1 i2 b1 b2 (c1 + 1) c2
     | _ -> () ];
  return ()
;

value print_prev_next_2 conf base ip sp i1 i2 b1 b2 c1 c2 pb nb =
  do match pb with
     [ Some b2 ->
         let sign = "&lt;&lt;" in
         print_sign conf sign ip sp i1 i2 b1 b2 c1 (c2 - 1)
     | _ -> () ];
     match (pb, nb) with
     [ (None, None) -> ()
     | _ -> Wserver.wprint "<font size=-1>%d</font>\n" c2 ];
     match nb with
     [ Some b2 ->
         let sign = "&gt;&gt;" in
         print_sign conf sign ip sp i1 i2 b1 b2 c1 (c2 + 1)
     | _ -> () ];
  return ()
;

value print_other_parent_if_same conf base ip b1 b2 =
  match (b1, b2) with
  [ ([(sib1, _) :: _], [(sib2, _) :: _]) ->
      match ((aoi base sib1).parents, (aoi base sib2).parents) with
      [ (Some ifam1, Some ifam2) ->
          let cpl1 = coi base ifam1 in
          let cpl2 = coi base ifam2 in
          let other_parent =
            if cpl1.father = ip then
              if cpl1.mother = cpl2.mother then Some cpl1.mother
              else None
            else
              if cpl1.father = cpl2.father then Some cpl1.father
              else None
          in
          match other_parent with
          [ Some ip ->
              do Wserver.wprint "&amp;\n";
                 print_someone conf base ip;
              return ()
          | _ -> () ]
      | _ -> () ]
  | _ -> () ]
;

value print_relation_ok conf base ip sp ip1 ip2 b1 b2 c1 c2 pb1 pb2 nb1 nb2 =
  let title _ =
    do Wserver.wprint "%s"
         (capitale
            (transl_nth conf "relationship link/relationship links" 0));
       match (pb1, nb1) with
       [ (None, None) -> ()
       | _ -> Wserver.wprint " %d" c1 ];
       match (pb2, nb2) with
       [ (None, None) -> ()
       | _ -> Wserver.wprint " %d" c2 ];
    return ()
  in
  do header_no_page_title conf title;
     if b1 = [] || b2 = [] then
       let b = if b1 = [] then b2 else b1 in
       do tag "center" begin
            print_someone conf base ip;
            print_spouse conf base ip b;
            html_br conf;
            print_one_branch conf base b;
          end;
          html_br conf;
          if b1 <> [] then
            print_prev_next_1 conf base ip sp ip1 ip2 b1 b2 c1 c2 pb1 nb1
          else
            print_prev_next_2 conf base ip sp ip1 ip2 b1 b2 c1 c2 pb2 nb2;
       return ()
     else
       tag "table" "cellspacing=0 cellpadding=0 width=\"100%%\"" begin
         do tag "tr" begin
              stag "td" "colspan=2 align=center" begin
                print_someone conf base ip;
                print_other_parent_if_same conf base ip b1 b2;
              end;
            end;
            tag "tr" begin
              stag "td" "colspan=2 align=center" begin
                Wserver.wprint "|";
              end;
            end;
            tag "tr" begin
              stag "td" "colspan=2 align=center" begin
                Wserver.wprint "<hr size=1 noshade%s>"
                  (if has_td_width_percent conf then " width=\"50%\""
                   else "");
              end;
            end;
            print_both_branches conf base b1 b2;
         return ();
         tag "tr" begin
           if b1 <> [] then
             tag "td" begin
               do html_br conf; return
               print_prev_next_1 conf base ip sp ip1 ip2 b1 b2 c1 c2
                 pb1 nb1;
             end
           else ();
           if b2 <> [] then
             tag "td" begin
               do html_br conf; return
               print_prev_next_2 conf base ip sp ip1 ip2 b1 b2 c1 c2
                 pb2 nb2;
             end
           else ();
         end;
       end;
     trailer conf;
  return ()
;

value print_relation conf base ip1 ip2 =
  let params =
    let po = find_person_in_env conf base "" in
    match (po, p_getint conf.env "l1", p_getint conf.env "l2") with
    [ (Some p, Some l1, Some l2) ->
        let ip = p.cle_index in        
        let dist = make_dist_tab conf base ip (max l1 l2 + 1) in
        let b1 = find_first_branch base dist ip l1 ip1 Neuter in
        let b2 = find_first_branch base dist ip l2 ip2 Neuter in
        Some (ip, (poi base ip).sex, dist, b1, b2, 1, 1)
    | _ ->
        match (p_getenv conf.env "b1", p_getenv conf.env "b2") with
        [ (Some b1str, Some b2str) ->
            let n1 = Num.of_string b1str in
            let n2 = Num.of_string b2str in
            match (branch_of_sosa base ip1 n1, branch_of_sosa base ip2 n2) with
            [ (Some [(ia1, sa1) :: b1], Some [(ia2, sa2) :: b2]) ->
                if ia1 == ia2 then
                  let c1 =
                    match p_getint conf.env "c1" with
                    [ Some n -> n
                    | None -> 0 ]
                  in
                  let c2 =
                    match p_getint conf.env "c2" with
                    [ Some n -> n
                    | None -> 0 ]
                  in
                  let dist =
                    if c1 > 0 || c2 > 0 then
                      let maxlev = max (List.length b1) (List.length b2) + 1 in
                      make_dist_tab conf base ia1 maxlev
                    else phony_dist_tab
                  in
                  Some (ia1, sa1, dist, Some b1, Some b2, c1, c2)
                else None
            | _ -> None ]
        | _ -> None ] ]
  in
  match params with
  [ Some (ip, sp, dist, Some b1, Some b2, c1, c2) ->
      let pb1 =
        if c1 <= 1 then None else find_prev_branch base dist ip sp b1
      in
      let nb1 =
        if c1 == 0 then None else find_next_branch base dist ip sp b1
      in
      let pb2 =
        if c2 <= 1 then None else find_prev_branch base dist ip sp b2
      in
      let nb2 =
        if c2 == 0 then None else find_next_branch base dist ip sp b2
      in
      print_relation_ok conf base ip sp ip1 ip2 b1 b2 c1 c2 pb1 pb2 nb1 nb2
  | _ ->
      let title _ = Wserver.wprint "Param&egrave;tres erron&eacute;s" in
      do header conf title;
         trailer conf;
      return () ]
;

value print conf base =
  match
    (find_person_in_env conf base "1", find_person_in_env conf base "2")
  with
  [ (Some p1, Some p2) -> print_relation conf base p1.cle_index p2.cle_index
  | _ -> incorrect_request conf ]
;
