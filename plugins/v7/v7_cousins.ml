open Geneweb.Config
open Gwdb
open Geneweb.Util
open Geneweb.Cousins
open Def

module BirthdayDisplay = Geneweb.BirthdayDisplay
module Cousins = Geneweb.Cousins
module DateDisplay = Geneweb.DateDisplay
module Gutil = Gutil
module Gwdb = Gwdb
module Hutil = Geneweb.Hutil
module Output = Geneweb.Output
module Util = Geneweb.Util

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
    if first then
      begin
        print_sosa conf base p2 true;
        Output.print_string conf
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

let rec print_descend_upto conf base cnt cnt_sp max_cnt ini_p ini_br lev children print print_sosa=
  if lev > 0 && cnt < max_cnt then
    begin
      if print then Output.print_string conf "<ul>\n";
      let (cnt, cnt_sp) =
	List.fold_left
	  (fun (cnt, cnt_sp) (ip, ia_asex, rev_br) ->
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
	       if (Array.length (get_family p)) = 1 && print then
		 let sp = get_spouse base ip (get_family p).(0) in
		 Printf.sprintf " %s %s" (Util.transl conf "with") (person_title_text conf base sp)
	       else ""
	     in
	     let br = List.rev ((ip, get_sex p) :: rev_br) in
	     let is_valid_rel = Cousins.br_inter_is_empty ini_br br in
	     if is_valid_rel && cnt < max_cnt && Cousins.has_desc_lev conf base lev p
	     then
	       begin
		 if lev <= 2 && print then
		   begin
		     Output.print_string conf "<li>";
		     if lev = 1 then
		       begin
			 give_access conf base ia_asex ini_p ini_br p br print_sosa;
		       end
		     else
		       let s =
			 let s = person_title_text conf base p in
			 transl_a_of_gr_eq_gen_lev conf
			   (transl_nth conf "child/children" 1)
			   s s
		       in
		       Output.printf conf "%s%s%s%s\n" (Utf8.capitalize_fst (Util.translate_eval s)) with_sp
			 (Util.transl conf ":") (if with_sp = "" then "<br>" else "")
		   end;
		 (* the function children_of returns *all* the children of ip *)
		 let (cnt, cnt_sp) =
		   List.fold_left
		     (fun (cnt, cnt_sp) ifam ->
			let children =
			  List.map
			    (fun ip ->
			       (ip, ia_asex, (get_iper p, get_sex p) :: rev_br))
			    (Cousins.children_of_fam base ifam)
			in
			let sp = get_spouse base ip ifam in
			if (Array.length (get_family p)) > 1 && lev >= 2 && print &&
			   ((List.length children) > 0) && (Cousins.has_desc_lev conf base lev sp)
			then
			  Output.printf conf "%s %s%s\n" (Util.transl conf "with")
			    (person_title_text conf base sp) (Util.transl conf ":") ;
			print_descend_upto conf base cnt cnt_sp max_cnt ini_p ini_br (lev - 1)
			  children print print_sosa;
		     )
		     (cnt, cnt_sp) (Array.to_list (get_family p))
		 in
		 if lev <= 2 && print then Output.print_string conf "</li>\n";
		 if lev = 1 then
		   let nb_sp = Array.length (get_family p) in
		   (cnt + 1, cnt_sp + nb_sp)
		 else (cnt, cnt_sp)
	       end
	     else
	       (cnt, cnt_sp)
	  )
	  (cnt, cnt_sp) children
      in
      if print then Output.print_string conf "</ul>\n";
      (cnt, cnt_sp)
    end
  else
    (cnt, cnt_sp)

let print_cousins_side_of conf base cnt cnt_sp max_cnt a ini_p ini_br lev1 lev2 print print_sosa =
  let sib = Cousins.siblings conf base (get_iper a) in
  if List.exists (Cousins.sibling_has_desc_lev conf base lev2) sib then
    begin
      if lev1 > 1 && print then
        begin
          Output.print_string conf "<li>\n";
          Output.printf conf "%s%s\n"
            (Utf8.capitalize_fst
               (cftransl conf "on %s's side"
                  [gen_person_title_text no_reference raw_access conf base
                     a]))
            (Util.transl conf ":")
        end;
      let sib = List.map (fun (ip, ia_asex) -> ip, ia_asex, []) sib in
      let (cnt, cnt_sp) =
	print_descend_upto conf base cnt cnt_sp max_cnt ini_p ini_br
	  lev2 sib print print_sosa
      in
      if lev1 > 1 && print then Output.print_string conf "</li>\n";
      (true, cnt, cnt_sp)
    end
  else (false, cnt, cnt_sp)

let print_cousins_lev conf base max_cnt p lev1 lev2 print print_sosa =
  let first_sosa =
    let rec loop sosa lev =
      if lev <= 1 then sosa else loop (Sosa.twice sosa) (lev - 1)
    in
    loop Sosa.one lev1
  in
  let last_sosa = Sosa.twice first_sosa in
  if print then
    begin
      Output.print_string conf "<div>\n";
      Util.print_tips_relationship conf;
      Output.print_string conf "</div>\n";
      if lev1 > 1 then Output.printf conf "<ul>\n";
    end;
  let (some, cnt, cnt_sp) =
    let rec loop sosa (some, cnt, cnt_sp) =
      if cnt < max_cnt && Sosa.gt last_sosa sosa then
        let (some, cnt, cnt_sp) =
          match Util.old_branch_of_sosa conf base (get_iper p) sosa with
            Some ((ia, _) :: _ as br) ->
            let (some1, cnt, cnt_sp) =
	      print_cousins_side_of conf base cnt cnt_sp max_cnt (pget conf base ia) p br
		lev1 lev2 print print_sosa
            in
            (some || some1, cnt, cnt_sp)
          | _ -> (some, cnt, cnt_sp)
        in
        loop (Sosa.inc sosa 1) (some, cnt, cnt_sp)
      else (some, cnt, cnt_sp)
    in
    loop first_sosa (false, 0, 0)
  in
  if some || not print then ()
  else Output.printf conf "%s.\n" (Utf8.capitalize_fst (transl conf "no match"));
  if lev1 > 1 && print then Output.print_string conf "</ul>\n";
  (cnt, cnt_sp)

let default_max_cousin_lev = 5
let default_max_cnt = Cousins.default_max_cnt

let brother_label conf x =
  match x with
    1 -> transl conf "siblings"
  | 2 -> transl conf "cousins"
  | 3 -> transl conf "2nd cousins"
  | 4 -> transl conf "3rd cousins"
  | n ->
    Printf.sprintf (ftransl conf "%s cousins")
      (transl_nth conf "nth (cousin)" (n - 1))

let cnt = ref 0
let cnt_sp = ref 0

let include_templ conf name =
  Util.include_template conf [] name
    (fun () -> Output.printf conf "Failed to open: %s.txt" name)

(* HTML main *)

let print_cousins conf base p lev1 lev2 =
  let title h =
    let txt_fun a =
      let txt = gen_person_text raw_access conf base p in
      transl_a_of_gr_eq_gen_lev conf a
        (if h then gen_person_text_no_html raw_access conf base p else txt)
        txt
    in
    if lev1 = lev2 then
      let s = txt_fun (brother_label conf lev1) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 2 && lev2 = 1 then
      let s = txt_fun (transl_nth conf "an uncle/an aunt" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 3 && lev2 = 1 then
      let s = txt_fun (transl_nth conf "a great-uncle/a great-aunt" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 2 then
      let s = txt_fun (transl_nth conf "a nephew/a niece" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 3 then
      let s = txt_fun (transl_nth conf "a great-nephew/a great-niece" 4) in
      Output.print_string conf (Utf8.capitalize_fst (Util.translate_eval s))
    else
      Output.printf conf "%s %d / %s %d" (Utf8.capitalize_fst (transl conf "ancestors")) lev1
        (Utf8.capitalize_fst (transl conf "descendants")) lev2
  in
  let max_cnt =
    try int_of_string (List.assoc "max_cousins" conf.base_env) with
      Not_found | Failure _ -> default_max_cnt
  in
  !V7_interp.notempl_with_menu title "perso_header" conf base p;
  Output.print_string conf "<div>\n";
  (*include_templ conf "cousins_tools";*)
  Output.print_string conf "<h3>\n";
  title false;
  Output.print_string conf "</h3>\n";
  Output.print_string conf "</div>\n";
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  let (cnt, cnt_sp) =
    print_cousins_lev conf base max_cnt p lev1 lev2 true V7_sosa.print_sosa
  in
  Output.print_string conf "<div>\n";
  Output.print_string conf "<p>\n";
  let (cnt2, cnt2_sp) =
    print_cousins_lev conf base max_cnt p lev1 lev2 false V7_sosa.print_sosa
  in
  if cnt >= max_cnt then Output.print_string conf "etc...\n"
  else if cnt > 1 then
    Output.printf conf "%s%s %d (%d) %s " (Utf8.capitalize_fst (transl conf "total"))
      (Util.transl conf ":") cnt cnt2
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
  if p_getenv conf.env "spouse" = Some "on" then
    Output.printf conf " %s %d (%d) %s.\n" (transl conf "and") cnt_sp cnt2_sp
      (Util.translate_eval ("@(c)" ^ transl_nth conf "spouse/spouses" 1))
  else Output.printf conf ".\n" ;
  Output.print_string conf "</p>\n";
  Output.print_string conf "</div>\n";
  Hutil.trailer conf

let print_anniv conf base p dead_people level =
  let module S = Map.Make (struct type t = iper let compare = compare end) in
  let s_mem x m = try let _ = S.find x m in true with Not_found -> false in
  let rec insert_desc set up_sosa down_br n ip =
    if s_mem ip set then set
    else
      let set = S.add ip (up_sosa, down_br) set in
      if n = 0 then set
      else
        let u = get_family (pget conf base ip) in
        let down_br = ip :: down_br in
        let rec loop set i =
          if i = Array.length u then set
          else
            let chil = get_children (foi base u.(i)) in
            let set =
              let rec loop set i =
                if i = Array.length chil then set
                else
                  let set =
                    insert_desc set up_sosa down_br (n - 1) chil.(i)
                  in
                  loop set (i + 1)
              in
              loop set 0
            in
            loop set (i + 1)
        in
        loop set 0
  in
  let set =
    let module P =
      Pqueue.Make
        (struct
          type t = iper * int * int
          let leq (_, lev1, _) (_, lev2, _) = lev1 <= lev2
        end)
    in
    let a = P.add (get_iper p, 0, 1) P.empty in
    let rec loop set a =
      if P.is_empty a then set
      else
        let ((ip, n, up_sosa), a) = P.take a in
        let set = insert_desc set up_sosa [] (n + 3) ip in
        if n >= level then set
        else
          let a =
            match get_parents (pget conf base ip) with
              Some ifam ->
              let cpl = foi base ifam in
              let n = n + 1 in
              let up_sosa = 2 * up_sosa in
              let a = P.add (get_father cpl, n, up_sosa) a in
              P.add (get_mother cpl, n, up_sosa + 1) a
            | None -> a
          in
          loop set a
    in
    loop S.empty a
  in
  let set =
    S.fold
      (fun ip (up_sosa, down_br) set ->
         let u = get_family (pget conf base ip) in
         let set = S.add ip (up_sosa, down_br, None) set in
         if Array.length u = 0 then set
         else
           let rec loop set i =
             if i = Array.length u then set
             else
               let cpl = foi base u.(i) in
               let c = Gutil.spouse ip cpl in
               loop (S.add c (up_sosa, down_br, Some ip) set) (i + 1)
           in
           loop set 0)
      set S.empty
  in
  let txt_of (up_sosa, down_br, spouse) conf base c =
    "<a href=\"" ^ commd conf ^ "m=RL&" ^ acces_n conf base "1" p ^ "&b1=" ^
    string_of_int up_sosa ^ "&" ^
    acces_n conf base "2"
      (match spouse with
         Some ip -> pget conf base ip
       | _ -> c) ^
    "&b2=" ^ string_of_int (sosa_of_persons conf base down_br) ^
    (match spouse with
       Some _ -> "&" ^ acces_n conf base "4" c
     | _ -> "") ^
    "&spouse=on\">" ^ person_title_text conf base c ^ "</a>"
  in
  let f_scan =
    let list = ref (S.fold (fun ip b list -> (ip, b) :: list) set []) in
    fun () ->
      match !list with
        (x, b) :: l -> list := l; pget conf base x, txt_of b
      | [] -> raise Not_found
  in
  let mode () =
    Output.print_string conf "<input type=\"hidden\" name=\"m\" value=\"C\">\n";
    Output.printf conf "<input type=\"hidden\" name=\"i\" value=\"%s\">\n"
      (string_of_iper (get_iper p));
    Output.printf conf "<input type=\"hidden\" name=\"t\" value=\"%s\">\n"
      (if dead_people then "AD" else "AN")
  in
  match p_getint conf.env "v" with
    Some i -> BirthdayDisplay.gen_print conf base i f_scan dead_people
  | _ ->
    if dead_people then BirthdayDisplay.gen_print_menu_dead conf base f_scan mode
    else BirthdayDisplay.gen_print_menu_birth conf base f_scan mode

let print conf base p =
  let max_lev =
    try int_of_string (List.assoc "max_cousins_level" conf.base_env) with
      Not_found | Failure _ -> default_max_cousin_lev
  in
  match (p_getint conf.env "v1", p_getint conf.env "v2", p_getenv conf.env "t") with
  | (Some 1, Some 1, _) | (Some 0, _, _)  | (_, Some 0, _)->
    !V7_interp.templ "cousins" conf base p
  | (Some lev1, _, _) ->
    let lev1 = min (max 1 lev1) max_lev in
    let lev2 =
      match p_getint conf.env "v2" with
        Some lev2 -> min (max 1 lev2) max_lev
      | None -> lev1
    in
    print_cousins conf base p lev1 lev2
  | (_, _, Some (("AN" | "AD") as t)) when conf.wizard || conf.friend ->
    print_anniv conf base p (t = "AD") max_lev
  | _ ->
    !V7_interp.templ "cousmenu" conf base p
