(* camlp4r ./pa_html.cmo *)
(* $Id: relationLink.ml,v 2.7 1999-08-01 08:50:14 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

(* Algorithm *)

type info =
  { ip : iper; sp : sex; ip1 : iper; ip2 : iper;
    b1 : list (iper * sex); b2 : list (iper * sex);
    c1 : int; c2 : int;
    pb1 : option (list (iper * sex)); pb2 : option (list (iper * sex));
    nb1 : option (list (iper * sex)); nb2 : option (list (iper * sex));
    sp1 : option person; sp2 : option person }
;

type dist =
  { dmin : mutable int;
    dmax : mutable int;
    mark : bool }
;

value infinity = 1000;

value threshold = ref 15;

value phony_dist_tab = (fun _ -> 0, fun _ -> infinity);

value tsort_leq tstab x y =
  if tstab.(x) = tstab.(y) then x >= y else tstab.(x) < tstab.(y)
;

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
           value leq x y = not (tsort_leq tstab x y);
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
            match find [(ip, sp) :: br] (len - 1) cpl.father Male with
            [ Some _ as r -> r
            | None -> find [(ip, sp) :: br] (len - 1) cpl.mother Female ]
        | None -> None ]
;

value rec next_branch_same_len base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    [ [] -> None
    | [(ip, sp) :: ipl1] ->
        match sa with
        [ Female ->
            next_branch_same_len base dist True (missing + 1) ip sp ipl1
        | Male ->
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                next_branch_same_len base dist False missing cpl.mother
                  Female ipl
            | _ -> failwith "next_branch_same_len" ]
        | Neuter -> assert False ] ]
  else if missing == 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    next_branch_same_len base dist True missing ia sa ipl
  else
    match (aoi base ia).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        next_branch_same_len base dist False (missing - 1) cpl.father Male
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
        [ Male ->
            prev_branch_same_len base dist True (missing + 1) ip sp ipl1
        | Female ->
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                prev_branch_same_len base dist False missing cpl.father
                  Male ipl
            | _ -> failwith "prev_branch_same_len" ]
        | Neuter -> assert False ] ]
  else if missing == 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    prev_branch_same_len base dist True missing ia sa ipl
  else
    match (aoi base ia).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        prev_branch_same_len base dist False (missing - 1) cpl.mother Female
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

value has_no_tables conf =
  let user_agent = Wserver.extract_param "user-agent: " '/' conf.request in
  String.lowercase user_agent = "lynx"
;

value text_size txt =
  let rec normal len i =
    if i = String.length txt then len
    else if txt.[i] = '<' then in_tag len (i + 1)
    else if txt.[i] = '&' then in_char (len + 1) (i + 1)
    else normal (len + 1) (i + 1)
  and in_tag len i =
    if i = String.length txt then len
    else if txt.[i] = '>' then normal len (i + 1)
    else in_tag len (i + 1)
  and in_char len i =
    if i = String.length txt then len
    else if txt.[i] = ';' then normal len (i + 1)
    else in_char len (i + 1)
  in
  normal 0 0
;

value print_pre_center sz txt =
  do for i = 1 to (sz - text_size txt) / 2 do Wserver.wprint " "; done;
     Wserver.wprint "%s\n" txt;
  return ()
;

value print_pre_left sz txt =
  let tsz = text_size txt in
  do if tsz < sz / 2 - 1 then
       for i = 2 to (sz / 2 - 1 - tsz) / 2 do Wserver.wprint " "; done
     else ();
     Wserver.wprint " %s\n" txt;
  return ()
;

value print_pre_right sz txt =
  let tsz = text_size txt in
  do if tsz < sz / 2 - 1 then
       do for i = 1 to sz / 2 do Wserver.wprint " "; done;
          for i = 1 to (sz / 2 - 1 - tsz) / 2 do Wserver.wprint " "; done;
       return ()
     else
       for i = 1 to sz - text_size txt - 1 do Wserver.wprint " "; done;
     Wserver.wprint " %s\n" txt;
  return ()
;

value someone_text conf base ip =
  let p = poi base ip in
  referenced_person_title_text conf base p ^
  Date.short_dates_text conf base p
;

value spouse_text conf base end_sp ip ipl =
  match (ipl, (p_getenv conf.env "spouse", p_getenv conf.env "opt")) with
  [ ([(ips, _) :: _], ((Some "on", _) | (_, Some "spouse"))) ->
      let a = aoi base ips in
      match a.parents with
      [ Some ifam ->
          let c = coi base ifam in
          let sp = if ip = c.father then c.mother else c.father in
          let d =
            match Adef.od_of_codate (foi base ifam).marriage with
            [ Some d -> "<font size=-2>" ^ Date.year_text d ^ "</font>"
            | None -> "" ]
          in
          (someone_text conf base sp, d)
      | _ -> ("", "") ]
  | ([], _) ->
      match end_sp with
      [ Some p -> (someone_text conf base p.cle_index, "")
      | _ -> ("", "") ]
  | _ -> ("", "") ]
;

value print_someone conf base ip =
  Wserver.wprint "%s\n" (someone_text conf base ip)
;

value print_spouse conf base n ip ipl =
  let (s, d) = spouse_text conf base n ip ipl in
  if s <> "" then
    do Wserver.wprint "&amp;%s" d;
       html_br conf;
       Wserver.wprint "%s\n" s;
    return ()
  else ()
;

value rec print_both_branches conf base info pl1 pl2 =
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
           | None -> Wserver.wprint "&nbsp;" ];
         end;
         stag "td" "align=center" begin
           match p2 with
           [ Some p2 -> Wserver.wprint "|"
           | None -> Wserver.wprint "&nbsp;" ];
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
                  print_spouse conf base info.sp1 p1 pl1;
               return ()
           | None -> Wserver.wprint "&nbsp;" ];
         end;
         tag "td" "valign=top align=center%s"
           (if has_td_width_percent conf then " width=\"50%\"" else "")
         begin
           match p2 with
           [ Some p2 ->
               do print_someone conf base p2;
                  print_spouse conf base info.sp2 p2 pl2;
               return ()
           | None -> Wserver.wprint "&nbsp;" ];
         end;
       end;
    return print_both_branches conf base info pl1 pl2
;

value rec print_both_branches_pre conf base info sz pl1 pl2 =
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
    do let s1 =
         match p1 with
         [ Some p1 -> "|"
         | None -> " " ]
       in
       let s2 =
         match p2 with
         [ Some p2 -> "|"
         | None -> " " ]
       in
       print_pre_center sz (s1 ^ String.make (sz / 2) ' ' ^ s2);
       match p1 with
       [ Some p1 ->
           do print_pre_left sz (someone_text conf base p1);
              let (s, d) = spouse_text conf base info.sp1 p1 pl1 in
              if s <> "" then print_pre_left sz ("&amp;" ^ d ^ " " ^ s)
              else ();
           return ()
       | None -> () ];
       match p2 with
       [ Some p2 ->
           do print_pre_right sz (someone_text conf base p2);
              let (s, d) = spouse_text conf base info.sp2 p2 pl2 in
              if s <> "" then print_pre_right sz ("&amp;" ^ d ^ " " ^ s)
              else ();
           return ()
       | None -> () ];
    return print_both_branches_pre conf base info sz pl1 pl2
;

value rec print_one_branch conf base ipl1 sp =
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
              print_spouse conf base sp ip1 ipl1;
              html_br conf;
           return ()
       | None -> () ];
    return print_one_branch conf base ipl1 sp
;

value include_marr conf base n =
  match find_person_in_env conf base n with
  [ Some p -> ";i" ^ n ^ "=" ^ string_of_int (Adef.int_of_iper p.cle_index)
  | None -> "" ]
;

value sign_text conf base sign info b1 b2 c1 c2 =
  "<a href=\"" ^ commd conf ^ "m=RL"
  ^ ";i1=" ^ string_of_int (Adef.int_of_iper info.ip1)
  ^ ";i2=" ^ string_of_int (Adef.int_of_iper info.ip2)
  ^ ";b1=" ^ Num.to_string (sosa_of_branch [(info.ip, info.sp) :: b1])
  ^ ";b2=" ^ Num.to_string (sosa_of_branch [(info.ip, info.sp) :: b2])
  ^ ";c1=" ^ string_of_int c1 ^ ";c2=" ^ string_of_int c2
  ^ (match p_getenv conf.env "spouse" with
     [ Some "on" -> ";spouse=on" | _ -> "" ])
  ^ include_marr conf base "3" ^ include_marr conf base "4"
  ^ "\">" ^ sign ^ "</a>"
;

value prev_next_1_text conf base info pb nb =
  let s =
    match pb with
    [ Some b1 ->
       let sign = "&lt;&lt;" in
       sign_text conf base sign info b1 info.b2 (info.c1 - 1) info.c2 ^ " "
    | _ -> "" ]
  in
  let s =
    match (pb, nb) with
    [ (None, None) -> s
    | _ -> s ^ "<font size=-1>" ^ string_of_int info.c1 ^ "</font>" ]
  in
  match nb with
  [ Some b1 ->
      let sign = "&gt;&gt;" in
      s ^ " " ^ sign_text conf base sign info b1 info.b2 (info.c1 + 1) info.c2
  | _ -> s ]
;

value prev_next_2_text conf base info pb nb =
  let s =
    match pb with
    [ Some b2 ->
       let sign = "&lt;&lt;" in
       sign_text conf base sign info info.b1 b2 info.c1 (info.c2 - 1) ^ " "
    | _ -> "" ]
  in
  let s =
    match (pb, nb) with
    [ (None, None) -> s
    | _ -> s ^ "<font size=-1>" ^ string_of_int info.c2 ^ "</font>" ]
  in
  match nb with
  [ Some b2 ->
     let sign = "&gt;&gt;" in
     s ^ " " ^ sign_text conf base sign info info.b1 b2 info.c1 (info.c2 + 1)
  | _ -> s ]
;

value print_prev_next_1 conf base info pb nb =
  Wserver.wprint "%s\n" (prev_next_1_text conf base info pb nb)
;

value print_prev_next_2 conf base info pb nb =
  Wserver.wprint "%s\n" (prev_next_2_text conf base info pb nb)
;

value other_parent_text_if_same conf base info =
  match (info.b1, info.b2) with
  [ ([(sib1, _) :: _], [(sib2, _) :: _]) ->
      match ((aoi base sib1).parents, (aoi base sib2).parents) with
      [ (Some ifam1, Some ifam2) ->
          let cpl1 = coi base ifam1 in
          let cpl2 = coi base ifam2 in
          let other_parent =
            if cpl1.father = info.ip then
              if cpl1.mother = cpl2.mother then Some cpl1.mother
              else None
            else
              if cpl1.father = cpl2.father then Some cpl1.father
              else None
          in
          match other_parent with
          [ Some ip ->
              let d =
                match Adef.od_of_codate (foi base ifam1).marriage with
                [ Some d -> "<font size=-2>" ^ Date.year_text d ^ "</font>"
                | None -> "" ]
              in
              "&amp;" ^ d ^ " " ^ someone_text conf base ip
          | _ -> "" ]
      | _ -> "" ]
  | _ -> "" ]
;

value print_other_parent_if_same conf base info =
  Wserver.wprint "%s" (other_parent_text_if_same conf base info)
;

value print_with_pre conf base info =
  let sz = 79 in
  tag "pre" begin
    print_pre_center sz (someone_text conf base info.ip);
    let s = other_parent_text_if_same conf base info in
    if s <> "" then print_pre_center sz s else ();
    print_pre_center sz "|";
    print_pre_center sz (String.make (sz / 2) '_');
    print_both_branches_pre conf base info sz info.b1 info.b2;
    if info.pb1 <> None || info.nb1 <> None
    || info.pb2 <> None || info.nb2 <> None
    then
      do Wserver.wprint "\n";
         if info.pb1 <> None || info.nb1 <> None then
           let s = prev_next_1_text conf base info info.pb1 info.nb1 in
           print_pre_left sz s
         else ();
         if info.pb2 <> None || info.nb2 <> None then
           let s = prev_next_2_text conf base info info.pb2 info.nb2 in
           print_pre_right sz s
         else ();
      return ()
    else ();
  end
;

value print_with_table conf base info =
  tag "table" "cellspacing=0 cellpadding=0 width=\"100%%\"" begin
    tag "tr" begin
      stag "td" "colspan=2 align=center" begin
        print_someone conf base info.ip;
        print_other_parent_if_same conf base info;
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
    print_both_branches conf base info info.b1 info.b2;
    if not conf.cancel_links &&
    (info.pb1 <> None || info.nb1 <> None
     || info.pb2 <> None || info.nb2 <> None)
    then
      tag "tr" begin
        tag "td" begin
          if info.pb1 <> None || info.nb1 <> None then
            do html_br conf; return
            print_prev_next_1 conf base info info.pb1 info.nb1
          else Wserver.wprint "&nbsp;";
        end;
        tag "td" begin
          if info.pb2 <> None || info.nb2 <> None then
            do html_br conf; return
            print_prev_next_2 conf base info info.pb2 info.nb2
          else Wserver.wprint "&nbsp;";
        end;
      end
    else ();
  end
;

value print_relation_path conf base info =
  if info.b1 = [] || info.b2 = [] then
    let b = if info.b1 = [] then info.b2 else info.b1 in
    let sp = if info.b1 = [] then info.sp2 else info.sp1 in
    do tag "center" begin
         print_someone conf base info.ip;
         print_spouse conf base sp info.ip b;
         html_br conf;
         print_one_branch conf base b sp;
       end;
       if not conf.cancel_links &&
       (info.pb1 <> None || info.nb1 <> None
        || info.pb2 <> None || info.nb2 <> None)
       then
         do html_br conf;
            if info.pb1 <> None || info.nb1 <> None then
              print_prev_next_1 conf base info info.pb1 info.nb1
            else ();
            if info.pb2 <> None || info.nb2 <> None then
              print_prev_next_2 conf base info info.pb2 info.nb2
            else ();
         return ()
       else ();
    return ()
  else if has_no_tables conf then
    print_with_pre conf base info
  else
    print_with_table conf base info
;

value print_relation_ok conf base info =
  let title _ =
    do Wserver.wprint "%s"
         (capitale
            (transl_nth conf "relationship link/relationship links" 0));
       match (info.pb1, info.nb1) with
       [ (None, None) -> ()
       | _ -> Wserver.wprint " %d" info.c1 ];
       match (info.pb2, info.nb2) with
       [ (None, None) -> ()
       | _ -> Wserver.wprint " %d" info.c2 ];
    return ()
  in
  do header_no_page_title conf title;
     print_relation_path conf base info;
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
      let sp1 = find_person_in_env conf base "3" in
      let sp2 = find_person_in_env conf base "4" in
      let info =
        {ip = ip; sp = sp; ip1 = ip1; ip2 = ip2; b1 = b1; b2 = b2;
         c1 = c1; c2 = c2; pb1 = pb1; pb2 = pb2; nb1 = nb1; nb2 = nb2;
         sp1 = sp1; sp2 = sp2}
      in
      print_relation_ok conf base info
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
