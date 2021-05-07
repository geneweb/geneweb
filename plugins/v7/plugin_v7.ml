open Geneweb
open Config
open Def
open Gwdb
open TemplAst
open Util

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


let get_descendants_at_level base p lev2 =
  match lev2 with
  | 0 -> []
  | n ->
    let rec loop acc lev fam =
      Array.fold_left
        (fun acc ifam ->
           let children = Array.to_list (get_children (foi base ifam)) in
           List.fold_left
             (fun acc ch ->
                if lev < n then
                  loop acc (lev + 1) (get_family (poi base ch))
                else
                if not (List.mem ifam acc) then
                  ifam :: acc
                else acc
             ) acc children
        ) acc fam
    in
    loop [] 1 (get_family p)

let count_cousins conf base p lev1 lev2 =
  match (lev1, lev2) with
  | (0, 0) -> 1 (* self *)
  | (0, lev2) -> (* descendants *)
    let ifam_l = get_descendants_at_level base p lev2 in
    List.fold_left begin fun cnt ifam ->
      foi base ifam
      |> get_children
      |> Array.length
      |> (+) cnt
    end 0 ifam_l
  | (_, _) ->
    let max_cnt =
      try int_of_string (List.assoc "max_cousins" conf.base_env)
      with Not_found | Failure _ -> Cousins.default_max_cnt
    in
    let () = Perso.build_sosa_ht conf base in
    print_cousins_lev conf base max_cnt p lev1 lev2 false Perso.print_sosa
    |> fst

let eval_simple_str_var = fun self conf base env (_, p_auth as ep) -> function
  | "desc_cnt" ->
    begin match Perso.get_env "desc_cnt" env with
      | Vint c -> string_of_int c
      | _ -> ""
    end
  | x -> Perso.eval_simple_str_var self conf base env ep x
and eval_compound_var self conf base env (a, _ as ep) loc = function
  | "descendant" :: sl ->
    begin match Perso.get_env "descendant" env with
      | Vind p ->
        let ep = p, authorized_age conf base p in
        self.Perso.eval_person_field_var self conf base env ep loc sl
      | _ -> raise Not_found
    end
  | "number_of_persons_at_level" :: sl | "number_of_descendants_at_level" :: sl ->
    begin match Perso.get_env "level" env with
      | Vint i ->
        begin match Perso.get_env "desc_level_table" env with
          | Vdesclevtab t ->
            let m = fst (Lazy.force t) in
            let cnt =
              Gwdb.Collection.fold begin fun cnt ip ->
                if Gwdb.Marker.get m ip = i then cnt + 1 else cnt
              end 0 (Gwdb.ipers base)
            in
            VVstring (self.Perso.eval_num self conf (Sosa.of_int cnt) sl)
          | _ -> raise Not_found
        end
      | _ -> raise Not_found
    end
  | x -> Perso.eval_compound_var self conf base env ep loc x
and eval_person_field_var self conf base env (p, p_auth as ep) loc = function
  | ["cousins"; l1; l2] ->
    let l1 = int_of_string l1 in
    let l2 = int_of_string l2 in
    VVstring (string_of_int (count_cousins conf base p l1 l2))
  | x -> Perso.eval_person_field_var self conf base env ep loc x


let print_foreach conf base
    (print_ast : ('a Templ.env -> 'b -> ast -> unit))
    (eval_expr : ('a Templ.env -> 'b -> ast -> string))
    =
    let rec print_foreach
        (env : 'a Templ.env)
        (ini_ep : 'b)
        (loc : loc)
        (s : string)
        (sl : string list)
        (ell : ast list list)
        (al : ast list)
      =
      match s with
      | "descendant" ->
        print_foreach_descendant conf base print_ast env al ini_ep
      | "family_at_level" ->
        print_foreach_family_at_level conf base print_ast env al ini_ep ini_ep
      | _ ->
        Perso.print_foreach conf base print_ast eval_expr env ini_ep loc s sl ell al
    and print_foreach_descendant conf base print_ast env al (p, _) =
      let lev = match Perso.get_env "level" env with Vint lev -> lev | _ -> 0 in
      let ifam_l = get_descendants_at_level base p lev in
      let ip_l =
        List.fold_left begin fun acc ifam ->
          Array.to_list (get_children (foi base ifam)) :: acc
        end [] ifam_l
        |> List.flatten
      in
      let rec loop i ip_l =
        match ip_l with
        | [] -> ()
        | ip :: ip_l ->
          let ep = (poi base ip), true in
          let env =
            ("descendant", Perso.Vind (poi base ip))
            :: ("desc_cnt", Vint i)
            :: ("first", Vbool (i=0))
            :: ("last", Vbool (ip_l=[] ))
            :: env
          in
          List.iter (print_ast env ep) al; loop (succ i) ip_l
      in
      loop 0 ip_l
    and print_foreach_family_at_level conf base print_ast env al ini_ep (p, _) =
      let lev =
        match Perso.get_env "level" env with
        | Vint lev -> lev
        | _ -> 0
      in
      let ifam_l = get_descendants_at_level base p lev in
      let rec loop prev i ifam_l =
        match ifam_l with
        | [] -> ()
        | ifam :: ifam_l ->
          let fam = foi base ifam in
          let ifath = get_father fam in
          let imoth = get_mother fam in
          let ispouse = Gutil.spouse (get_iper p) fam in
          let cpl = ifath, imoth, ispouse in
          let m_auth = authorized_age conf base (pget conf base ifath)
                       && authorized_age conf base (pget conf base imoth)
          in
          let vfam = Perso.Vfam (ifam, fam, cpl, m_auth) in
          let env =
            ("#loop", Perso.Vint 0)
            :: ("fam", vfam)
            :: ("family_cnt", Perso.Vint (i + 1))
            :: env
          in
          let env =
            match prev with
            | Some vfam -> ("prev_fam", vfam) :: env
            | None -> env
          in
          List.iter (print_ast env ini_ep) al; loop (Some vfam) (i + 1) ifam_l
      in
      loop None 0 ifam_l
    in
    print_foreach
