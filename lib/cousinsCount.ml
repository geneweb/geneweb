open Config
open Def
open Gwdb
open Util

let cnt = ref 0
let cnt_sp = ref 0

let give_access conf base ia_asex p1 b1 p2 b2 print_sosa =
  let reference _ _ p s =
    if is_hidden p then s
    else
      "<a href=\"" ^ commd conf ^ "m=RL&" ^ acces_n conf base "1" p1 ^
      "&b1=" ^ Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b1)) ^ "&" ^
      acces_n conf base "2" p2 ^ "&b2=" ^
      Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b2)) ^ "&spouse=" ^
      (if p_getenv conf.env "spouse" = Some "on" then "on" else "") ^
      "&image=" ^
      (if p_getenv conf.env "image" = Some "off" then "off" else "") ^ "&bd=" ^
      (match p_getenv conf.env "bd" with
         Some x -> x
       | None -> "0") ^
      "\">" ^ s ^ "</a>"
  in
  let reference_sp p3 _ _ p s =
    if is_hidden p then s
    else
      "<a href=\"" ^ commd conf ^ "m=RL&" ^ acces_n conf base "1" p1 ^
      "&b1=" ^ Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b1)) ^ "&" ^
      acces_n conf base "2" p2 ^ "&b2=" ^
      Sosa.to_string (Util.old_sosa_of_branch conf base (ia_asex :: b2)) ^ "&" ^
      acces_n conf base "4" p3 ^ "&spouse=" ^
      (if p_getenv conf.env "spouse" = Some "on" then "on" else "") ^
      "&image=" ^
      (if p_getenv conf.env "image" = Some "off" then "off" else "") ^ "&bd=" ^
      (match p_getenv conf.env "bd" with
         Some x -> x
       | None -> "0") ^
      "\">" ^ s ^ "</a>"
  in
  let print_nospouse _ =
    print_sosa conf base p2 true;
    Output.printf conf "%s%s"
      (gen_person_title_text reference std_access conf base p2)
      (DateDisplay.short_dates_text conf base p2)
  in
  let print_spouse sp first =
    incr cnt_sp;
    if first then
      begin
        print_sosa conf base p2 true;
        Output.printf conf "%s"
          (gen_person_title_text reference std_access conf base p2)
      end
    else Output.printf conf "<br>%s" (person_title_text conf base p2);
    Output.printf conf "%s &amp; " (DateDisplay.short_dates_text conf base p2);
    print_sosa conf base sp true;
    Output.printf conf "%s%s"
      (gen_person_title_text (reference_sp sp) std_access conf base sp)
      (DateDisplay.short_dates_text conf base sp)
  in
  if p_getenv conf.env "spouse" = Some "on" then begin
    match get_family p2 with
    | [||] -> print_nospouse ()
    | u ->
      Array.iteri
        (fun i ifam ->
           let cpl = foi base ifam in
           let sp =
             if get_sex p2 = Female then pget conf base (get_father cpl)
             else pget conf base (get_mother cpl)
           in
           print_spouse sp (i = 0))
        u
  end
  else print_nospouse ()

let rec print_descend_upto conf base max_cnt ini_p ini_br lev children display print_sosa =
  if lev > 0 && !cnt < max_cnt then
    begin
      if display then Output.print_string conf "<ul>\n";
      List.iter
        (fun (ip, ia_asex, rev_br) ->
           let p = pget conf base ip in
           (* détecter l'époux de p, parent des enfants qui seront listés *)
           let get_spouse base iper ifam =
             let f = foi base ifam in
             if iper = get_father f then poi base (get_mother f)
             else poi base (get_father f)
           in
           (* if more than one spouse, this will be split on multiple lines *)
           (* we ignore the case where two spouses, but only one with descendants! *)
           let with_sp =
             if (Array.length (get_family p)) = 1 && display then
               let sp = get_spouse base ip (get_family p).(0) in
               Printf.sprintf " %s %s" (Util.transl conf "with") (person_title_text conf base sp)
             else ""
           in
           let br = List.rev ((ip, get_sex p) :: rev_br) in
           let is_valid_rel = Cousins.br_inter_is_empty ini_br br in
           if is_valid_rel && !cnt < max_cnt && Cousins.has_desc_lev conf base lev p
           then
             begin
               if display && lev = 1 then incr cnt
               else if lev <= 2 then
                 begin
                   Output.print_string conf "<li>";
                   if lev = 1 then
                     begin
                       give_access conf base ia_asex ini_p ini_br p br print_sosa;
                       incr cnt
                     end
                   else
                     let s =
                       let s = person_title_text conf base p in
                       transl_a_of_gr_eq_gen_lev conf
                         (transl_nth conf "child/children" 1)
                         s s
                     in
                     Output.printf conf "%s%s%s%s\n" (Utf8.capitalize (Util.translate_eval s)) with_sp
                       (Util.transl conf ":") (if with_sp = "" then "<br>" else "")
                 end;
               (* the function children_of returns *all* the children of ip *)
               Array.iter
                 (fun ifam ->
                    let children =
                      List.map
                        (fun ip ->
                           (ip, ia_asex, (get_iper p, get_sex p) :: rev_br))
                        (Cousins.children_of_fam base ifam)
                    in
                    let sp = get_spouse base ip ifam in
                    if (Array.length (get_family p)) > 1 && lev >= 2 &&
                       ((List.length children) > 0) && (Cousins.has_desc_lev conf base lev sp)
                    then
                      if display then Output.printf conf "%s %s%s\n" (Util.transl conf "with")
                        (person_title_text conf base sp) (Util.transl conf ":") ;
                    print_descend_upto conf base max_cnt ini_p ini_br (lev - 1) children display print_sosa;
                 )
                 (get_family p) ;
               if lev <= 2 && display then Output.print_string conf "</li>\n"
             end)
        children;
      if display then Output.print_string conf "</ul>\n"
    end

let print_cousins_side_of conf base max_cnt a ini_p ini_br lev1 lev2 display print_sosa =
  let sib = Cousins.siblings conf base (get_iper a) in
  if List.exists (Cousins.sibling_has_desc_lev conf base lev2) sib then
    begin
      if lev1 > 1 && display then
        begin
          Output.print_string conf "<li>\n";
          Output.printf conf"%s%s\n"
            (Utf8.capitalize
               (cftransl conf "on %s's side"
                  [gen_person_title_text no_reference raw_access conf base
                     a]))
            (Util.transl conf ":")
        end;
      let sib = List.map (fun (ip, ia_asex) -> ip, ia_asex, []) sib in
      print_descend_upto conf base max_cnt ini_p ini_br lev2 sib display print_sosa;
      if lev1 > 1 && display then Output.print_string conf "</li>\n";
      true
    end
  else false

let print_cousins_lev conf base max_cnt p lev1 lev2 display print_sosa =
  let first_sosa =
    let rec loop sosa lev =
      if lev <= 1 then sosa else loop (Sosa.twice sosa) (lev - 1)
    in
    loop Sosa.one lev1
  in
  let last_sosa = Sosa.twice first_sosa in
  if display then
  begin
  	Output.print_string conf "<div>\n";
  	Util.print_tips_relationship conf;
  	Output.print_string conf "</div>\n";
  	if lev1 > 1 then Output.print_string conf "<ul>\n";
  end;
  let some =
    let rec loop sosa some =
      if !cnt < max_cnt && Sosa.gt last_sosa sosa then
        let some =
          match Util.old_branch_of_sosa conf base (get_iper p) sosa with
            Some ((ia, _) :: _ as br) ->
            print_cousins_side_of conf base max_cnt (pget conf base ia) p br
              lev1 lev2 display print_sosa ||
            some
          | _ -> some
        in
        loop (Sosa.inc sosa 1) some
      else some
    in
    loop first_sosa false
  in
  if some then ()
  else Output.printf conf "%s.\n" (Utf8.capitalize (transl conf "no match"));
  if lev1 > 1 then Output.print_string conf "</ul>\n"
