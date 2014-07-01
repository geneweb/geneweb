(* camlp5r ./pa_html.cmo *)
(* $Id: relationLink.ml,v 5.20 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open Util;

(* Algorithm *)

type info =
  { ip : iper;
    sp : sex;
    ip1 : iper;
    ip2 : iper;
    b1 : list (iper * sex);
    b2 : list (iper * sex);
    c1 : int;
    c2 : int;
    pb1 : option (list (iper * sex));
    pb2 : option (list (iper * sex));
    nb1 : option (list (iper * sex));
    nb2 : option (list (iper * sex));
    sp1 : option person;
    sp2 : option person;
    bd : int;
    td_prop : string }
;

type dist = { dmin : mutable int; dmax : mutable int; mark : bool };

value infinity = 1000;

value threshold = ref 10;

value phony_dist_tab = (fun _ -> 0, fun _ -> infinity);

value tsort_leq tstab x y =
  if tstab.(x) = tstab.(y) then x >= y else tstab.(x) < tstab.(y)
;

value make_dist_tab conf base ia maxlev =
  if maxlev <= threshold.val then phony_dist_tab
  else
    (* optimization to be used 1/ if database not too big or 2/ running
    on machines with much memory *)
(*
    let () = load_unions_array base in
    let () = load_descends_array base in
*)
    let tstab = Util.create_topological_sort conf base in
    let module Pq =
      Pqueue.Make
        (struct type t = int; value leq x y = not (tsort_leq tstab x y); end)
    in
    let default = {dmin = infinity; dmax = 0; mark = False} in
    let dist = Array.create (nb_of_persons base) default in
    let q = ref Pq.empty in
    let add_children ip =
      let u = pget conf base ip in
      for i = 0 to Array.length (get_family u) - 1 do {
        let des = foi base (get_family u).(i) in
        for j = 0 to Array.length (get_children des) - 1 do {
          let k = Adef.int_of_iper (get_children des).(j) in
          let d = dist.(k) in
          if not d.mark then do {
            dist.(k) := {dmin = infinity; dmax = 0; mark = True};
            q.val := Pq.add k q.val
          }
          else ()
        }
      }
    in
    do {
      dist.(Adef.int_of_iper ia) := {dmin = 0; dmax = 0; mark = True};
      add_children ia;
      while not (Pq.is_empty q.val) do {
        let (k, nq) = Pq.take q.val in
        q.val := nq;
        match get_parents (pget conf base (Adef.iper_of_int k)) with
        [ Some ifam ->
            let cpl = foi base ifam in
            let dfath = dist.(Adef.int_of_iper (get_father cpl)) in
            let dmoth = dist.(Adef.int_of_iper (get_mother cpl)) in
            do {
              dist.(k).dmin := min dfath.dmin dmoth.dmin + 1;
              dist.(k).dmax := max dfath.dmax dmoth.dmax + 1;
              if dist.(k).dmin > maxlev then ()
              else add_children (Adef.iper_of_int k)
            }
        | None -> () ]
      };
      (fun ip -> dist.(Adef.int_of_iper ip).dmin,
       fun ip -> dist.(Adef.int_of_iper ip).dmax)
    }
;

value find_first_branch conf base (dmin, dmax) ia =
  find [] where rec find br len ip sp =
    if ip = ia then if len = 0 then Some br else None
    else if len = 0 then None
    else if len < dmin ip || len > dmax ip then None
    else
      match get_parents (pget conf base ip) with
      [ Some ifam ->
          let cpl = foi base ifam in
          match find [(ip, sp) :: br] (len - 1) (get_father cpl) Male with
          [ Some _ as r -> r
          | None -> find [(ip, sp) :: br] (len - 1) (get_mother cpl) Female ]
      | None -> None ]
;

value rec next_branch_same_len conf base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    [ [] -> None
    | [(ip, sp) :: ipl1] ->
        match sa with
        [ Female ->
            next_branch_same_len conf base dist True (missing + 1) ip sp ipl1
        | Male ->
            match get_parents (pget conf base ip) with
            [ Some ifam ->
                let cpl = foi base ifam in
                next_branch_same_len conf base dist False missing
                  (get_mother cpl) Female ipl
            | _ -> failwith "next_branch_same_len" ]
        | Neuter -> assert False ] ]
  else if missing = 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    next_branch_same_len conf base dist True missing ia sa ipl
  else
    match get_parents (pget conf base ia) with
    [ Some ifam ->
        let cpl = foi base ifam in
        next_branch_same_len conf base dist False (missing - 1)
          (get_father cpl) Male [(ia, sa) :: ipl]
    | None -> next_branch_same_len conf base dist True missing ia sa ipl ]
;

value find_next_branch conf base dist ia sa ipl =
  loop ia sa ipl where rec loop ia1 sa1 ipl =
    match next_branch_same_len conf base dist True 0 ia1 sa1 ipl with
    [ Some (ia1, sa1, ipl) -> if ia = ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None ]
;

value rec prev_branch_same_len conf base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    [ [] -> None
    | [(ip, sp) :: ipl1] ->
        match sa with
        [ Male ->
            prev_branch_same_len conf base dist True (missing + 1) ip sp ipl1
        | Female ->
            match get_parents (pget conf base ip) with
            [ Some ifam ->
                let cpl = foi base ifam in
                prev_branch_same_len conf base dist False missing
                  (get_father cpl) Male ipl
            | _ -> failwith "prev_branch_same_len" ]
        | Neuter -> assert False ] ]
  else if missing = 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    prev_branch_same_len conf base dist True missing ia sa ipl
  else
    match get_parents (pget conf base ia) with
    [ Some ifam ->
        let cpl = foi base ifam in
        prev_branch_same_len conf base dist False (missing - 1)
          (get_mother cpl) Female [(ia, sa) :: ipl]
    | None -> prev_branch_same_len conf base dist True missing ia sa ipl ]
;

value find_prev_branch conf base dist ia sa ipl =
  loop ia sa ipl where rec loop ia1 sa1 ipl =
    match prev_branch_same_len conf base dist True 0 ia1 sa1 ipl with
    [ Some (ia1, sa1, ipl) -> if ia = ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None ]
;

(* Printing *)

value someone_text conf base ip =
  let p = pget conf base ip in
  referenced_person_title_text conf base p ^ Date.short_dates_text conf base p
;

value spouse_text conf base end_sp ip ipl =
  match (ipl, (p_getenv conf.env "spouse", p_getenv conf.env "opt")) with
  [ ([(ips, _) :: _], (Some "on", _) | (_, Some "spouse")) ->
      let a = pget conf base ips in
      match get_parents a with
      [ Some ifam ->
          let fam = foi base ifam in
          let sp =
            if ip = get_father fam then get_mother fam else get_father fam
          in
          let d =
            Date.short_marriage_date_text conf base fam
              (pget conf base (get_father fam))
              (pget conf base (get_mother fam))
          in
          (someone_text conf base sp, d, Some sp)
      | _ -> ("", "", None) ]
  | ([], _) ->
      match end_sp with
      [ Some p ->
          (someone_text conf base (get_key_index p), "",
           Some (get_key_index p))
      | _ -> ("", "", None) ]
  | _ -> ("", "", None) ]
;

value print_someone_and_spouse conf base info in_tab ip n ipl =
  let (s, d, spo) = spouse_text conf base n ip ipl in
  do {
    if in_tab && (info.bd > 0 || info.td_prop <> "") then
      Wserver.wprint "<table style=\"border:%dpx solid\"><tr><td align=\"center\"%s>"
        info.bd info.td_prop
    else ();
    Wserver.wprint "%s\n" (someone_text conf base ip);
    Wserver.wprint "%s" (Dag.image_txt conf base (pget conf base ip));
    if s <> "" then do {
      xtag "br";
      Wserver.wprint "&amp;%s" d;
      Wserver.wprint " %s\n" s;
      match spo with
      [ Some ip ->
          Wserver.wprint "%s" (Dag.image_txt conf base (pget conf base ip))
      | _ -> () ]
    }
    else ();
    if in_tab && (info.bd > 0 || info.td_prop <> "") then
      Wserver.wprint "</td></tr></table>"
    else ();
  }
;

value rec print_both_branches conf base info pl1 pl2 =
  if pl1 = [] && pl2 = [] then ()
  else do {
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
    tag "tr" "align=\"%s\"" conf.left begin
      stag "td" "align=\"center\"" begin
        match p1 with
        [ Some p1 -> Wserver.wprint "|"
        | None -> Wserver.wprint "&nbsp;" ];
      end;
      stag "td" begin Wserver.wprint "&nbsp;"; end;
      stag "td" "align=\"center\"" begin
        match p2 with
        [ Some p2 -> Wserver.wprint "|"
        | None -> Wserver.wprint "&nbsp;" ];
      end;
      Wserver.wprint "\n";
    end;
    tag "tr" "align=\"%s\"" conf.left begin
      tag "td" "valign=\"top\" align=\"center\"" begin
        match p1 with
        [ Some p1 ->
            print_someone_and_spouse conf base info True p1 info.sp1 pl1
        | None -> Wserver.wprint "&nbsp;" ];
      end;
      tag "td" begin Wserver.wprint "&nbsp;"; end;
      tag "td" "valign=\"top\" align=\"center\"" begin
        match p2 with
        [ Some p2 ->
            print_someone_and_spouse conf base info True p2 info.sp2 pl2
        | None -> Wserver.wprint "&nbsp;" ];
      end;
    end;
    print_both_branches conf base info pl1 pl2
  }
;

value rec print_both_branches_pre conf base info sz pl1 pl2 =
  if pl1 = [] && pl2 = [] then ()
  else do {
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
    let s1 =
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
        do {
          print_pre_left sz (someone_text conf base p1);
          let (s, d, _) = spouse_text conf base info.sp1 p1 pl1 in
          if s <> "" then print_pre_left sz ("&amp;" ^ d ^ " " ^ s) else ()
        }
    | None -> Wserver.wprint "\n" ];
    match p2 with
    [ Some p2 ->
        do {
          print_pre_right sz (someone_text conf base p2);
          let (s, d, _) = spouse_text conf base info.sp2 p2 pl2 in
          if s <> "" then print_pre_right sz ("&amp;" ^ d ^ " " ^ s) else ()
        }
    | None -> Wserver.wprint "\n" ];
    print_both_branches_pre conf base info sz pl1 pl2
  }
;

value include_marr conf base n =
  match find_person_in_env conf base n with
  [ Some p -> ";" ^ acces_n conf base n p
  | None -> "" ]
;

value sign_text conf base sign info b1 b2 c1 c2 =
  "<a href=\"" ^ commd conf ^ "m=RL" ^ ";" ^
    acces_n conf base "1" (pget conf base info.ip1) ^ ";" ^
    acces_n conf base "2" (pget conf base info.ip2) ^ ";b1=" ^
    Num.to_string (sosa_of_branch [(info.ip, info.sp) :: b1]) ^ ";b2=" ^
    Num.to_string (sosa_of_branch [(info.ip, info.sp) :: b2]) ^ ";c1=" ^
    string_of_int c1 ^ ";c2=" ^ string_of_int c2 ^
    (match p_getenv conf.env "spouse" with
     [ Some "on" -> ";spouse=on"
     | _ -> "" ]) ^
    (match p_getenv conf.env "image" with
     [ Some "on" -> ";image=on"
     | _ -> "" ]) ^
    (match p_getenv conf.env "bd" with
     [ None | Some ("0" | "") -> ""
     | Some x -> ";bd=" ^ x ]) ^
    (match p_getenv conf.env "td" with
     [ None | Some "" -> ""
     | Some x -> ";td=" ^ x ]) ^
    (match p_getenv conf.env "color" with
     [ None | Some "" -> ""
     | Some x -> ";color=" ^ code_varenv x ]) ^
    include_marr conf base "3" ^ include_marr conf base "4" ^ "\">" ^ sign ^
    "</a>"
;

value prev_next_1_text conf base info pb nb =
  let s =
    match pb with
    [ Some b1 ->
        let sign = "&lt;&lt;" in
        sign_text conf base sign info b1 info.b2 (info.c1 - 1) info.c2 ^ "\n"
    | _ -> "" ]
  in
  let s =
    match (pb, nb) with
    [ (None, None) -> s
    | _ ->
        s ^ "<span style=\"font-size:80%\">" ^ string_of_int info.c1 ^
        "</span>" ]
  in
  match nb with
  [ Some b1 ->
      let sign = "&gt;&gt;" in
      s ^ "\n" ^ sign_text conf base sign info b1 info.b2 (info.c1 + 1) info.c2
  | _ -> s ]
;

value prev_next_2_text conf base info pb nb =
  let s =
    match pb with
    [ Some b2 ->
        let sign = "&lt;&lt;" in
        sign_text conf base sign info info.b1 b2 info.c1 (info.c2 - 1) ^ "\n"
    | _ -> "" ]
  in
  let s =
    match (pb, nb) with
    [ (None, None) -> s
    | _ ->
        s ^ "<span style=\"font-size:80%\">" ^ string_of_int info.c2 ^
        "</span>" ]
  in
  match nb with
  [ Some b2 ->
      let sign = "&gt;&gt;" in
      s ^ "\n" ^ sign_text conf base sign info info.b1 b2 info.c1 (info.c2 + 1)
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
      match
        (get_parents (pget conf base sib1), get_parents (pget conf base sib2))
      with
      [ (Some ifam1, Some ifam2) ->
          let cpl1 = foi base ifam1 in
          let cpl2 = foi base ifam2 in
          let other_parent =
            if get_father cpl1 = info.ip then
              if get_mother cpl1 = get_mother cpl2 then
                Some (get_mother cpl1)
              else None
            else if get_father cpl1 = get_father cpl2 then
              Some (get_father cpl1)
            else None
          in
          match other_parent with
          [ Some ip ->
              let d =
                Date.short_marriage_date_text conf base (foi base ifam1)
                  (pget conf base (get_father cpl1))
                  (pget conf base (get_mother cpl1))
              in
              Some ("&amp;" ^ d ^ " " ^ someone_text conf base ip, ip)
          | _ -> None ]
      | _ -> None ]
  | _ -> None ]
;

value print_someone_and_other_parent_if_same conf base info =
  do {
    if info.bd > 0 || info.td_prop <> "" then
      Wserver.wprint "<table style=\"border:%dpx solid\"><tr><td align=\"center\"%s>"
        info.bd info.td_prop
    else ();
    Wserver.wprint "%s\n" (someone_text conf base info.ip);
    Wserver.wprint "%s" (Dag.image_txt conf base (pget conf base info.ip));
    match other_parent_text_if_same conf base info with
    [ Some (s, ip) ->
        do {
          xtag "br";
          Wserver.wprint "%s" s;
          Wserver.wprint "%s" (Dag.image_txt conf base (pget conf base ip))
        }
    | None -> () ];
    if info.bd > 0 || info.td_prop <> "" then
      Wserver.wprint "</td></tr></table>"
    else ();
  }
;

value rec list_iter_hd_tl f =
  fun
  [ [x :: l] -> do { f x l; list_iter_hd_tl f l }
  | [] -> () ]
;

value print_one_branch_no_table conf base info =
  let b = if info.b1 = [] then info.b2 else info.b1 in
  let sp = if info.b1 = [] then info.sp2 else info.sp1 in
  tag "div" "style=\"text-align:center\"" begin
    print_someone_and_spouse conf base info False info.ip sp b;
    xtag "br";
    list_iter_hd_tl
      (fun (ip1, _) ipl1 ->
         do {
           Wserver.wprint "|";
           xtag "br";
           print_someone_and_spouse conf base info False ip1 sp ipl1;
           xtag "br";
         })
      b;
  end
;

value print_one_branch_with_table conf base info =
  let b = if info.b1 = [] then info.b2 else info.b1 in
  let sp = if info.b1 = [] then info.sp2 else info.sp1 in
  tag "table"
    "border=\"%d\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%%\""
    conf.border
  begin
    tag "tr" begin
      tag "td" "align=\"center\"" begin
        print_someone_and_spouse conf base info True info.ip sp b;
      end;
      list_iter_hd_tl
        (fun (ip1, _) ipl1 ->
           do {
             tag "tr" begin
               tag "td" "align=\"center\"" begin
                 Wserver.wprint "|";
               end;
             end;
             tag "tr" begin
               tag "td" "align=\"center\"" begin
                 print_someone_and_spouse conf base info True ip1 sp ipl1;
               end;
             end;
           })
        b;
    end;
  end
;

value print_two_branches_with_pre conf base info =
  let sz = 79 in
  tag "pre" begin
    print_pre_center sz (someone_text conf base info.ip);
    match other_parent_text_if_same conf base info with
    [ Some (s, ip) -> print_pre_center sz s
    | None -> () ];
    print_pre_center sz "|";
    print_pre_center sz (String.make (sz / 2) '_');
    print_both_branches_pre conf base info sz info.b1 info.b2;
    if info.pb1 <> None || info.nb1 <> None || info.pb2 <> None ||
       info.nb2 <> None then
       do {
      Wserver.wprint "\n";
      if info.pb1 <> None || info.nb1 <> None then
        let s = prev_next_1_text conf base info info.pb1 info.nb1 in
        print_pre_left sz s
      else ();
      if info.pb2 <> None || info.nb2 <> None then
        let s = prev_next_2_text conf base info info.pb2 info.nb2 in
        print_pre_right sz s
      else ()
    }
    else ();
  end
;

value print_two_branches_with_table conf base info =
  tag "table"
    "border=\"%d\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%%\""
    conf.border
  begin
    tag "tr" "align=\"%s\"" "left" begin
      stag "td" "colspan=\"3\" align=\"center\"" begin
        print_someone_and_other_parent_if_same conf base info;
      end;
    end;
    tag "tr" "align=\"%s\"" "left" begin
      stag "td" "colspan=\"3\" align=\"center\"" begin Wserver.wprint "|"; end;
    end;
    tag "tr" "align=\"%s\"" "left" begin
      stagn "td" "align=\"%s\"" conf.right begin
        xtag "hr" "class=\"%s\"" conf.right;
      end;
      stagn "td" begin
        xtag "hr" "class=\"full\"";
      end;
      stagn "td" "align=\"%s\"" conf.left begin
        xtag "hr" "class=\"%s\"" conf.left;
      end;
    end;
    print_both_branches conf base info info.b1 info.b2;
    if not conf.cancel_links &&
       (info.pb1 <> None || info.nb1 <> None || info.pb2 <> None ||
        info.nb2 <> None) then
      tag "tr" "align=\"%s\"" "left" begin
        tag "td" begin
          if info.pb1 <> None || info.nb1 <> None then do {
            xtag "br"; print_prev_next_1 conf base info info.pb1 info.nb1
          }
          else Wserver.wprint "&nbsp;";
        end;
        tag "td" begin Wserver.wprint "&nbsp;"; end;
        tag "td" begin
          if info.pb2 <> None || info.nb2 <> None then do {
            xtag "br"; print_prev_next_2 conf base info info.pb2 info.nb2
          }
          else Wserver.wprint "&nbsp;";
        end;
      end
    else ();
  end
;

value print_relation_path conf base info =
  let with_table =
    match p_getenv conf.env "tab" with
    [ Some "on" -> True
    | Some "off" -> False
    | _ -> not (browser_doesnt_have_tables conf) ]
  in
  if info.b1 = [] || info.b2 = [] then do {
    if (info.bd > 0 || info.td_prop <> "") && with_table then
      print_one_branch_with_table conf base info
    else print_one_branch_no_table conf base info;
    if not conf.cancel_links &&
       (info.pb1 <> None || info.nb1 <> None || info.pb2 <> None ||
        info.nb2 <> None) then
       do {
      tag "p" begin
        if info.pb1 <> None || info.nb1 <> None then
          print_prev_next_1 conf base info info.pb1 info.nb1
        else ();
        if info.pb2 <> None || info.nb2 <> None then
          print_prev_next_2 conf base info info.pb2 info.nb2
        else ();
      end
    }
    else ()
  }
  else if with_table then print_two_branches_with_table conf base info
  else print_two_branches_with_pre conf base info
;

value print_relation_ok conf base info =
  let title _ =
    do {
      Wserver.wprint "%s"
        (capitale (transl_nth conf "relationship link/relationship links" 0));
      match (info.pb1, info.nb1) with
      [ (None, None) -> ()
      | _ -> Wserver.wprint " %d" info.c1 ];
      match (info.pb2, info.nb2) with
      [ (None, None) -> ()
      | _ -> Wserver.wprint " %d" info.c2 ]
    }
  in
  do {
    header_no_page_title conf title;
    print_link_to_welcome conf True;
    xtag "p" "style=\"clear:both\"";
    print_relation_path conf base info;
    trailer conf
  }
;

value print_relation_no_dag conf base po ip1 ip2 =
  let params =
    match (po, p_getint conf.env "l1", p_getint conf.env "l2") with
    [ (Some p, Some l1, Some l2) ->
        let ip = get_key_index p in
        let dist = make_dist_tab conf base ip (max l1 l2 + 1) in
        let b1 = find_first_branch conf base dist ip l1 ip1 Neuter in
        let b2 = find_first_branch conf base dist ip l2 ip2 Neuter in
        Some (ip, get_sex (pget conf base ip), dist, b1, b2, 1, 1)
    | _ ->
        match (p_getenv conf.env "b1", p_getenv conf.env "b2") with
        [ (Some b1str, Some b2str) ->
            let n1 = Num.of_string b1str in
            let n2 = Num.of_string b2str in
            match
              (branch_of_sosa conf base ip1 n1,
               branch_of_sosa conf base ip2 n2)
            with
            [ (Some [(ia1, sa1) :: b1], Some [(ia2, sa2) :: b2]) ->
                if ia1 = ia2 then
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
                      let maxlev =
                        max (List.length b1) (List.length b2) + 1
                      in
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
        if c1 <= 1 then None else find_prev_branch conf base dist ip sp b1
      in
      let nb1 =
        if c1 = 0 then None else find_next_branch conf base dist ip sp b1
      in
      let pb2 =
        if c2 <= 1 then None else find_prev_branch conf base dist ip sp b2
      in
      let nb2 =
        if c2 = 0 then None else find_next_branch conf base dist ip sp b2
      in
      let sp1 = find_person_in_env conf base "3" in
      let sp2 = find_person_in_env conf base "4" in
      let bd = match p_getint conf.env "bd" with [ Some x -> x | None -> 0 ] in
      let td_prop =
        match Util.p_getenv conf.env "td" with
        [ Some x -> " " ^ x
        | _ ->
            match Util.p_getenv conf.env "color" with
            [ None | Some "" -> ""
            | Some x -> " class=\"" ^ x ^ "\"" ] ]
      in
      let info =
        {ip = ip; sp = sp; ip1 = ip1; ip2 = ip2; b1 = b1; b2 = b2; c1 = c1;
         c2 = c2; pb1 = pb1; pb2 = pb2; nb1 = nb1; nb2 = nb2; sp1 = sp1;
         sp2 = sp2; bd = bd; td_prop = td_prop}
      in
      print_relation_ok conf base info
  | _ -> incorrect_request conf ]
;

value print_relation_dag conf base a ip1 ip2 l1 l2 =
  let ia = get_key_index a in
  let add_branches dist set n ip l =
    let b = find_first_branch conf base dist ia l ip Neuter in
    let rec loop set n b =
      if n > 100 then raise Exit
      else
        match b with
        [ Some b ->
            let set =
              List.fold_left (fun set (ip, _) -> Dag.Pset.add ip set) set b
            in
            loop set (n + 1)
              (find_next_branch conf base dist ia (get_sex a) b)
        | None -> (set, n) ]
    in
    loop set n b
  in
  try
    let set =
      List.fold_left
        (fun set l1 ->
           List.fold_left
             (fun set l2 ->
                let dist = make_dist_tab conf base ia (max l1 l2 + 1) in
                let (set, n) = add_branches dist set 0 ip1 l1 in
                let (set, n) = add_branches dist set n ip2 l2 in
                set)
             set l2)
        (Dag.Pset.add ia Dag.Pset.empty) l1
    in
    let spl =
      List.fold_right
        (fun (ip, s) spl ->
           match find_person_in_env conf base s with
           [ Some sp -> [(ip, (get_key_index sp, None)) :: spl]
           | None -> spl ])
        [(ip1, "3"); (ip2, "4")] []
    in
    let elem_txt p = Dag.Item p "" in
    let vbar_txt ip = "" in
    let invert =
      match Util.p_getenv conf.env "invert" with
      [ Some "on" -> True
      | _ -> False ]
    in
    let page_title = Util.capitale (Util.transl conf "tree") in
    Dag.make_and_print_dag conf base elem_txt vbar_txt invert set spl
      page_title ""
  with
  [ Exit -> Hutil.incorrect_request conf ]
;

value int_list s =
  loop 0 0 where rec loop i n =
    if i = String.length s then [n]
    else
      match s.[i] with
      [ '0'..'9' as d -> loop (i + 1) (n * 10 + Char.code d - Char.code '0')
      | _ -> [n :: loop (i + 1) 0] ]
;

value print_relation conf base p1 p2 =
  let l1 = p_getenv conf.env "l1" in
  let l2 = p_getenv conf.env "l2" in
  let po = find_person_in_env conf base "" in
  match (p_getenv conf.env "dag", po, l1, l2) with
  [ (Some "on", Some p, Some l1, Some l2) ->
      print_relation_dag conf base p (get_key_index p1) (get_key_index p2)
        (int_list l1) (int_list l2)
  | _ ->
      print_relation_no_dag conf base po (get_key_index p1)
        (get_key_index p2) ]
;

value print conf base = do {
  Wserver.wrap_string.val := Util.xml_pretty_print;
  match
    (find_person_in_env conf base "1", find_person_in_env conf base "2")
  with
  [ (Some p1, Some p2) -> print_relation conf base p1 p2
  | _ -> incorrect_request conf ]
};
