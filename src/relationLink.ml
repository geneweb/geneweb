(* camlp4r ./pa_html.cmo *)
(* $Id: relationLink.ml,v 1.2 1998-11-04 13:52:33 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

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
           [ Some p1 -> print_someone conf base p1
           | None -> () ];
         end;
         tag "td" "valign=top align=center%s"
           (if has_td_width_percent conf then " width=\"50%\"" else "")
         begin
           match p2 with
           [ Some p2 -> print_someone conf base p2
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
    do tag "tr" begin
         stag "td" "align=center" begin
           match ip1 with
           [ Some ip1 -> Wserver.wprint "|"
           | None -> () ];
         end;
         Wserver.wprint "\n";
       end;
       tag "tr" begin
         tag "td" "align=center" begin
           match ip1 with
           [ Some ip1 -> print_someone conf base ip1
           | None -> () ];
         end;
       end;
    return print_one_branch conf base ipl1
;

type dist =
  { dmin : mutable int;
    dmax : mutable int;
    mark : bool }
;

value infinity = 1000;

value threshold = ref 15;

value leq = ref (fun []);
module Pq =
  Pqueue.Make (struct type t = int; value leq x y = leq.val x y; end)
;

value make_dist_tab base ia maxlev =
  if maxlev <= threshold.val then
    (fun _ -> 0, fun _ -> infinity)
  else
    let _ = base.ascends.array () in
    let _ = base.couples.array () in
    let id = Consang.topological_sort base in
    let default = {dmin = infinity; dmax = 0; mark = False} in
    let dist = Array.create base.persons.len default in
    do leq.val := fun x y -> id.(x) > id.(y); return
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
         match (base.ascends.get k).parents with
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
            match find [(ip, sp) :: br] (len - 1) cpl.father Masculin with
            [ Some _ as r -> r
            | None -> find [(ip, sp) :: br] (len - 1) cpl.mother Feminin ]
        | None -> None ]
;

value branch_of_num base ip n =
  let rec expand bl n =
    if Num.eq n Num.one then bl else expand [Num.even n :: bl] (Num.half n)
  in
  let rec loop ipl ip sp =
    fun
    [ [] -> [(ip, sp) :: ipl]
    | [goto_fath :: nl] ->
        match (aoi base ip).parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            if goto_fath then loop [(ip, sp) :: ipl] cpl.father Masculin nl
            else loop [(ip, sp) :: ipl] cpl.mother Feminin nl
        | _ -> [(ip, sp) :: ipl] ] ]
  in
  loop [] ip (poi base ip).sexe (expand [] n)
;

value num_of_branch ia sa ipl =
  let ipl = List.tl (List.rev [(ia, sa) :: ipl]) in
  List.fold_left
    (fun b (ip, sp) ->
       let b = Num.twice b in
       match sp with
       [ Masculin -> b
       | Feminin -> Num.inc b 1
       | Neutre -> assert False ])
    Num.one ipl
;

value rec next_branch_same_len base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    [ [] -> None
    | [(ip, sp) :: ipl1] ->
        match sa with
        [ Feminin ->
            next_branch_same_len base dist True (missing + 1) ip sp ipl1
        | Masculin ->
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                next_branch_same_len base dist False missing cpl.mother
                  Feminin ipl
            | _ -> failwith "next_branch_same_len" ]
        | Neutre -> assert False ] ]
  else if missing == 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    next_branch_same_len base dist True missing ia sa ipl
  else
    match (aoi base ia).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        next_branch_same_len base dist False (missing - 1) cpl.father Masculin
          [(ia, sa) :: ipl]
    | None -> next_branch_same_len base dist True missing ia sa ipl ]
;

value text_of_sex =
  fun
  [ Masculin -> "Masculin"
  | Feminin -> "Feminin"
  | Neutre -> "Neutre" ]
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
        [ Masculin ->
            prev_branch_same_len base dist True (missing + 1) ip sp ipl1
        | Feminin ->
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                prev_branch_same_len base dist False missing cpl.father
                  Masculin ipl
            | _ -> failwith "prev_branch_same_len" ]
        | Neutre -> assert False ] ]
  else if missing == 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    prev_branch_same_len base dist True missing ia sa ipl
  else
    match (aoi base ia).parents with
    [ Some ifam ->
        let cpl = coi base ifam in
        prev_branch_same_len base dist False (missing - 1) cpl.mother Feminin
          [(ia, sa) :: ipl]
    | None -> prev_branch_same_len base dist True missing ia sa ipl ]
;

value find_prev_branch base dist ia sa ipl =
  loop ia sa ipl where rec loop ia1 sa1 ipl =
    match prev_branch_same_len base dist True 0 ia1 sa1 ipl with
    [ Some (ia1, sa1, ipl) -> if ia == ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None ]
;

value print_sign conf sign ip sp i1 i2 b1 b2 c1 c2 =
  do Wserver.wprint "<a href=%sm=RL;i1=%d;i2=%d" (commd conf)
       (Adef.int_of_iper i1) (Adef.int_of_iper i2);
     Wserver.wprint ";b1=%s" (Num.to_string (num_of_branch ip sp b1));
     Wserver.wprint ";b2=%s" (Num.to_string (num_of_branch ip sp b2));
     Wserver.wprint ";c1=%d;c2=%d" c1 c2;
     Wserver.wprint ">%s</a>" sign;
     Wserver.wprint "\n";
  return ()
;

value print_prev_next conf base ip sp i1 i2 b1 b2 c1 c2 pb1 pb2 nb1 nb2 =
  tag "tr" begin
    if b1 <> [] then
      tag "td" begin
        Wserver.wprint "<br>\n";
        match pb1 with
        [ Some b1 ->
            let sign = "&lt;&lt;" in
            print_sign conf sign ip sp i1 i2 b1 b2 (c1 - 1) c2
        | _ -> () ];
        match (pb1, nb1) with
        [ (None, None) -> ()
        | _ -> Wserver.wprint "<font size=-1>%d</font>\n" c1 ];
        match nb1 with
        [ Some b1 ->
            let sign = "&gt;&gt;" in
            print_sign conf sign ip sp i1 i2 b1 b2 (c1 + 1) c2
        | _ -> () ];
      end
    else ();
    if b2 <> [] then
      tag "td" begin
        Wserver.wprint "<br>\n";
        match pb2 with
        [ Some b2 ->
            let sign = "&lt;&lt;" in
            print_sign conf sign ip sp i1 i2 b1 b2 c1 (c2 - 1)
        | _ -> () ];
        match (pb2, nb2) with
        [ (None, None) -> ()
        | _ -> Wserver.wprint "<font size=-1>%d</font>\n" c2 ];
        match nb2 with
        [ Some b2 ->
            let sign = "&gt;&gt;" in
            print_sign conf sign ip sp i1 i2 b1 b2 c1 (c2 + 1)
        | _ -> () ];
      end
    else ();
  end
;

value print_relation conf base ip1 ip2 =
  let params =
    let po = find_person_in_env conf base "" in
    match (po, p_getint conf.env "l1", p_getint conf.env "l2") with
    [ (Some p, Some l1, Some l2) ->
        let ip = p.cle_index in        
        let dist = make_dist_tab base ip (max l1 l2 + 1) in
        let b1 = find_first_branch base dist ip l1 ip1 Neutre in
        let b2 = find_first_branch base dist ip l2 ip2 Neutre in
        Some (ip, (poi base ip).sexe, dist, b1, b2, 1, 1)
    | _ ->
        match
          (p_getenv conf.env "b1", p_getenv conf.env "b2",
           p_getint conf.env "c1", p_getint conf.env "c2")
        with
        [ (Some b1str, Some b2str, Some c1, Some c2) ->
            let n1 = Num.of_string b1str in
            let n2 = Num.of_string b2str in
            match (branch_of_num base ip1 n1, branch_of_num base ip2 n2) with
            [ ([(ia1, sa1) :: b1], [(ia2, sa2) :: b2]) ->
                if ia1 == ia2 then
                  let dist =
                    let maxlev = max (List.length b1) (List.length b2) + 1 in
                    make_dist_tab base ia1 maxlev
                  in
                  Some (ia1, sa1, dist, Some b1, Some b2, c1, c2)
                else None
            | _ -> None ]
        | _ -> None ] ]
  in
  match params with
  [ Some (ip, sp, dist, Some b1, Some b2, c1, c2) ->
      let pb1 =
         if c1 == 1 then None else find_prev_branch base dist ip sp b1
      in
      let nb1 = find_next_branch base dist ip sp b1 in
      let pb2 =
        if c2 == 1 then None else find_prev_branch base dist ip sp b2
      in
      let nb2 = find_next_branch base dist ip sp b2 in
      let title _ =
        do Wserver.wprint "Lien de parent&eacute;";
           match (pb1, nb1) with
           [ (None, None) -> ()
           | _ -> Wserver.wprint " %d" c1 ];
           match (pb2, nb2) with
           [ (None, None) -> ()
           | _ -> Wserver.wprint " %d" c2 ];
        return ()
      in
      do Util.html conf;
         Wserver.wprint "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \
 \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n";
         tag "head" begin
           Wserver.wprint "  <meta name=\"ROBOTS\" content=\"NONE\">\n";
           Wserver.wprint "  <title>";
           title True;
           Wserver.wprint "</title>\n";
         end;
         Wserver.wprint "\n";
         Wserver.wprint "<body%s>\n"
           (try " " ^ List.assoc "body_prop" conf.base_env with
            [ Not_found -> "" ]);
         tag "table" "cellspacing=0 cellpadding=0 width=\"100%%\"" begin
           if b1 = [] || b2 = [] then
             let b = if b1 = [] then b2 else b1 in
             do tag "tr" begin
                  stag "td" "align=center" begin
                    print_someone conf base ip;
                  end;
                end;
                print_one_branch conf base b;
             return ()
           else
             do tag "tr" begin
                  stag "td" "colspan=2 align=center" begin
                    print_someone conf base ip;
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
           print_prev_next conf base ip sp ip1 ip2 b1 b2 c1 c2 pb1 pb2 nb1 nb2;
         end;
         trailer conf;
      return ()
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
