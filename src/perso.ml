(* camlp4r ./pa_html.cmo *)
(* $Id: perso.ml,v 4.1 2001-03-17 05:54:21 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value max_im_wid = 240;
value max_im_hei = 240;
value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

value has_children base u =
  List.exists
    (fun ifam ->
       let des = doi base ifam in
       Array.length des.children > 0)
    (Array.to_list u.family)
;

value print_marriage_text conf base in_perso fam =
  let marriage = Adef.od_of_codate fam.marriage in
  let marriage_place = sou base fam.marriage_place in
  do if in_perso then
       match (marriage, marriage_place) with
       [ (None, "") -> ()
       | _ -> Wserver.wprint "<em>" ]
     else ();
     match marriage with
     [ Some d -> Wserver.wprint " %s" (Date.string_of_ondate conf d)
     | _ -> () ];
     match marriage_place with
     [ "" -> ()
     | s -> Wserver.wprint ", %s," s ];
     if in_perso then
       match (marriage, marriage_place) with
       [ (None, "") -> ()
       | _ -> Wserver.wprint "</em>" ]
     else ();
  return ()
;

value print_title conf base and_txt p (nth, name, title, places, dates) =
  do let href =
       "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
       code_varenv (sou base (List.hd places))
     in
     let (tit, est) = (sou base title, sou base (List.hd places)) in
     let s = tit ^ " " ^ est in
     wprint_geneweb_link conf href s;
     let rec loop places =
       do match places with
          [ [] -> ()
          | [_] -> Wserver.wprint "\n%s " and_txt
          | _ -> Wserver.wprint ",\n" ];
       return
       match places with
       [ [place :: places] ->
           let href =
             "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
             code_varenv (sou base place)
           in
           let est = sou base place in
           do wprint_geneweb_link conf href est;
           return loop places
       | _ -> () ]
     in
     loop (List.tl places);
  return
  let paren =
    match (nth, dates, name) with
    [ (n, _, _) when n > 0 -> True
    | (_, _, Tname _) -> True
    | (_, [(Some _, _) :: _], _) -> age_autorise conf base p
    | _ -> False ]
  in
  do if paren then Wserver.wprint "\n(" else (); return
  let first =
    if nth > 0 then
      do Wserver.wprint "%s"
           (if nth >= 100 then string_of_int nth
            else transl_nth conf "nth" nth);
      return False
    else True
  in
  let first =
    match name with
    [ Tname n ->
        do if not first then Wserver.wprint " ," else ();
           Wserver.wprint "%s" (sou base n);
        return False
    | _ -> first ]
  in
  do if age_autorise conf base p && dates <> [(None, None)] then
       let _ =
         List.fold_left
           (fun first (date_start, date_end) ->
              do if not first then Wserver.wprint ",\n" else ();
                 match date_start with
                 [ Some d -> Wserver.wprint "%s" (Date.string_of_date conf d)
                 | None -> () ];
                 match date_end with
                 [ Some (Dgreg d _) ->
                     do if d.month <> 0 then Wserver.wprint " - "
                        else Wserver.wprint "-";
                     return ()
                 | _ -> () ];
                 match date_end with
                 [ Some d -> Wserver.wprint "%s" (Date.string_of_date conf d)
                 | None -> () ];
              return False)
           first dates
       in
       ()
     else ();
     if paren then Wserver.wprint ")" else ();
  return ()
;

value name_equiv n1 n2 =
  n1 = n2 || n1 = Tmain && n2 = Tnone || n1 = Tnone && n2 = Tmain
;

value nobility_titles_list conf p =
  let titles =
    List.fold_right
      (fun t l ->
         let t_date_start = Adef.od_of_codate t.t_date_start in
         let t_date_end = Adef.od_of_codate t.t_date_end in
         match l with
         [ [(nth, name, title, place, dates) :: rl]
           when
             not conf.is_rtl &&
             nth = t.t_nth && name_equiv name t.t_name && title = t.t_ident &&
             place = t.t_place ->
             [(nth, name, title, place,
               [(t_date_start, t_date_end) :: dates]) ::
              rl]
         | _ ->
             [(t.t_nth, t.t_name, t.t_ident, t.t_place,
               [(t_date_start, t_date_end)]) ::
              l] ])
      p.titles []
  in
  List.fold_right
    (fun (t_nth, t_name, t_ident, t_place, t_dates) l ->
       match l with
       [ [(nth, name, title, places, dates) :: rl]
         when
           not conf.is_rtl &&
           nth = t_nth && name_equiv name t_name && title = t_ident &&
           dates = t_dates ->
           [(nth, name, title, [t_place :: places], dates) :: rl]
       | _ -> [(t_nth, t_name, t_ident, [t_place], t_dates) :: l] ])
    titles []
;

(* obsolete; should be removed one day *)

value print_titles conf base cap and_txt p =
  let titles = nobility_titles_list conf p in
  list_iter_first
    (fun first t ->
       do if not first then Wserver.wprint "," else ();
          Wserver.wprint "\n";
          print_title conf base and_txt p t;
       return ())
    titles
;

(* Version matching the Sosa number of the "ancestor" pages *)

value find_sosa_aux conf base a p =
  let tstab = Util.create_topological_sort conf base in
  let mark = Array.create base.data.persons.len False in
  let rec gene_find =
    fun
    [ [] -> Left []
    | [(z, ip) :: zil] ->
        if ip = a.cle_index then Right z
        else if mark.(Adef.int_of_iper ip) then gene_find zil
        else
          do mark.(Adef.int_of_iper ip) := True; return
          if tstab.(Adef.int_of_iper a.cle_index) <=
               tstab.(Adef.int_of_iper ip) then
            gene_find zil
          else
            let asc = aoi base ip in
            match asc.parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                let z = Num.twice z in
                match gene_find zil with
                [ Left zil ->
                    Left [(z, cpl.father); (Num.inc z 1, cpl.mother) :: zil]
                | Right z -> Right z ]
            | None -> gene_find zil ] ]
  in
  find [(Num.one, p.cle_index)] where rec find zil =
    match gene_find zil with
    [ Left [] -> None
    | Left zil -> find zil
    | Right z -> Some (z, p) ]
;
(* Male version
value find_sosa_aux conf base a p =
  let mark = Array.create base.data.persons.len False in
  let rec find z ip =
    if ip = a.cle_index then Some z
    else if mark.(Adef.int_of_iper ip) then None
    else
      do mark.(Adef.int_of_iper ip) := True; return
      let asc = aoi base ip in
      match asc.parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let z = Num.twice z in
          match find z cpl.father with
          [ Some z -> Some z
          | None -> find (Num.inc z 1) cpl.mother ]
      | None -> None ]
  in
  find Num.one p.cle_index
;
*)

value find_sosa conf base a =
  match Util.find_person_in_env conf base "z" with
  [ Some p ->
      if a.cle_index = p.cle_index then Some (Num.one, p)
      else
        let u = uoi base a.cle_index in
	if has_children base u then find_sosa_aux conf base a p
	else None
  | None -> None ]
;

(* Interpretation of template file 'perso.txt' *)

type ast = Templ.ast ==
  [ Atext of string
  | Avar of string and list string
  | Atransl of bool and string and char
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of string and list string and list ast
  | Adefine of string and list string and list ast and list ast
  | Aapply of string and list ast_expr ]
and ast_expr = Templ.ast_expr ==
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Eop of string and ast_expr and ast_expr
  | Enot of ast_expr
  | Estr of string
  | Eint of string
  | Evar of string and list string
  | Etransl of bool and string and char ]
;

type env =
  [ Vind of person and ascend and union
  | Vfam of family and (iper * iper) and descend
  | Vrel of relation
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vsosa of Lazy.t (option (Num.t * person))
  | Vimage of Lazy.t (option (string * option (option (int * int))))
  | Vtitle of title_item
  | Vnone ]
and title_item =
  (int * gen_title_name istr * istr * list istr *
   list (option date * option date))
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value eval_variable conf base env sl =
  let ep =
    match (get_env "p" env, get_env "p_auth" env) with
    [ (Vind p a u, Vbool p_auth) -> (p, a, u, p_auth)
    | _ -> assert False ]
  in
  let efam = get_env "fam" env in
  let make_ep ip =
    let p = poi base ip in
    let a = aoi base ip in
    let u = uoi base ip in
    let p_auth = age_autorise conf base p in
    (p, a, u, p_auth)
  in
  loop ep efam sl where rec loop (p, a, u, p_auth) efam =
    fun
    [ ["child" :: sl] ->
        match get_env "child" env with
        [ Vind p a u ->
            let auth =
              match get_env "auth" env with
              [ Vbool True -> age_autorise conf base p
              | _ -> False ]
            in
            let ep = (p, a, u, auth) in
            loop ep efam sl
        | _ -> None ]
    | ["father" :: sl] ->
        match a.parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            let ep = make_ep cpl.father in
            let cpl = (cpl.father, cpl.mother) in
            let efam = Vfam (foi base ifam) cpl (doi base ifam) in
            loop ep efam sl
        | None -> None ]
    | ["mother" :: sl] ->
        match a.parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            let ep = make_ep cpl.mother in
            let cpl = (cpl.mother, cpl.father) in
            let efam = Vfam (foi base ifam) cpl (doi base ifam) in
            loop ep efam sl
        | None -> None ]
    | ["self" :: sl] -> loop (p, a, u, p_auth) efam sl
    | ["spouse" :: sl] ->
        match efam with
        [ Vfam fam (_, ip) _ ->
            let ep = make_ep ip in
            loop ep efam sl
        | _ -> None ]
    | ["witness" :: sl] ->
        match get_env "witness" env with
        [ Vind p a u ->
            let ep = (p, a, u, age_autorise conf base p) in
            loop ep efam sl
        | _ -> None ]
    | [] -> Some ((p, a, u, p_auth), efam, "")
    | [s] -> Some ((p, a, u, p_auth), efam, s)
    | _ -> None ]
;

value print_age conf base env p p_auth =
  if p_auth then
    match (Adef.od_of_codate p.birth, p.death) with
    [ (Some (Dgreg d _), NotDead) ->
        let a = temps_ecoule d conf.today in
        Date.print_age conf a
    | _ -> () ]
  else ()
;

value print_alias conf base env =
  match get_env "alias" env with
  [ Vstring s -> Wserver.wprint "%s" s
  | _ -> () ]
;

value print_baptism_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.baptism_place) else ()
;

value print_birth_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.birth_place) else ()
;

value print_burial_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.burial_place) else ()
;

value print_comment conf base env p p_auth =
  fun
  [ Vfam fam _ _ ->
      if p_auth then copy_string_with_macros conf [] (sou base fam.comment)
      else ()
  | _ -> () ]
;

value print_consanguinity conf base env a p_auth =
  if p_auth then
    do print_decimal_num conf
         (round_2_dec (Adef.float_of_fix a.consang *. 100.0));
       Wserver.wprint "%%";
    return ()
  else ()
;

value print_death_age conf base env p p_auth =
  if p_auth then
    match Date.get_birth_death_date p with
    [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
       Some (Dgreg ({prec = Sure | About | Maybe} as d2) _),
       approx) when d1 <> d2 ->
        let a = temps_ecoule d1 d2 in
        do if not approx && d1.prec = Sure && d2.prec = Sure then ()
           else
             Wserver.wprint "%s " (transl_decline conf "possibly (date)" "");
           Date.print_age conf a;
        return ()
    | _ -> () ]
  else ()
;

value print_death_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.death_place) else ()
;

value print_died conf base env p p_auth =
  if p_auth then
    let is = index_of_sex p.sex in
    match p.death with
    [ Death dr _ ->
        let dr_w =
          match dr with
          [ Unspecified -> transl_nth conf "died" is
          | Murdered -> transl_nth conf "murdered" is
          | Killed -> transl_nth conf "killed (in action)" is
          | Executed -> transl_nth conf "executed (legally killed)" is
          | Disappeared -> transl_nth conf "disappeared" is ]
        in
        Wserver.wprint "%s" (capitale dr_w)
    | DeadYoung ->
        Wserver.wprint "%s" (capitale (transl_nth conf "died young" is))
    | DeadDontKnowWhen ->
        Wserver.wprint "%s" (capitale (transl_nth conf "died" is))
    | _ -> () ]
  else ()
;

value print_divorce_date conf base env p p_auth =
  fun
  [ Vfam fam (_, isp) _ ->
      match fam.divorce with
      [ Divorced d ->
          let d = Adef.od_of_codate d in
          let auth =
            let spouse = poi base isp in
            p_auth && age_autorise conf base spouse
          in
          match d with
          [ Some d when auth ->
              do Wserver.wprint " <em>";
                 Wserver.wprint "%s" (Date.string_of_ondate conf d);
                 Wserver.wprint "</em>";
              return ()
          | _ -> () ]
      | _ -> () ]
  | _ -> () ]
;

value print_first_name_alias conf base env =
  match get_env "first_name_alias" env with
  [ Vstring s -> Wserver.wprint "%s" s
  | _ -> () ]
;  

value print_first_name_key conf base env p p_auth =
  Wserver.wprint "%s" (code_varenv (Name.lower (p_first_name base p)))
;

value print_image_size conf base env p p_auth =
  if p_auth then
    match get_env "image" env with
    [ Vimage x ->
        match Lazy.force x with
        [ Some (_, Some (Some (width, height))) ->
            Wserver.wprint " width=%d height=%d" width height
        | Some (link, None) ->
            Wserver.wprint " height=%d" max_im_hei
        | None | Some (_, Some None) -> () ]
    | _ -> () ]
  else ()
;

value print_image_url conf base env p p_auth =
  if p_auth then
    match get_env "image" env with
    [ Vimage x ->
        match Lazy.force x with
        [ Some (fname, Some (Some _)) ->
            let s = Unix.stat fname in
            let b = acces conf base p in
            let k = default_image_name base p in
            Wserver.wprint "%sm=IM;d=%d;%s;k=/%s" (commd conf)
              (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
              b k
        | Some (link, None) ->
            Wserver.wprint "%s" link
        | None | Some (_, Some None) -> () ]
    | _ -> () ]
  else ()
;

value print_married_to conf base env p p_auth =
  fun
  [ Vfam fam (_, ispouse) des ->
      let spouse = poi base ispouse in
      let auth = p_auth && age_autorise conf base spouse in
      let format = relation_txt conf p.sex fam in
      Wserver.wprint (fcapitale format)
        (fun _ ->
           if auth then print_marriage_text conf base True fam else ())
  | _ -> () ]
;

value print_nobility_title conf base env p p_auth =
  match get_env "nobility_title" env with
  [ Vtitle t when p_auth -> print_title conf base (transl conf "and") p t
  | _ -> () ]
;

value obsolete_list = ref [];

value obsolete var new_var =
  if List.mem var obsolete_list.val then ()
  else ifdef UNIX then
    do Printf.eprintf "\
*** <W> perso.txt: variable \"%%%s;\" obsolete; use rather \"%%%s;\"\n"
          var new_var;
       flush stderr;
       obsolete_list.val := [var :: obsolete_list.val];
    return ()
  else ()
;

value print_nobility_titles conf base env p p_auth =
  do obsolete "nobility_titles" "nobility_title"; return
  if p_auth then print_titles conf base True (transl conf "and") p else ()
;

value print_notes conf base env p p_auth =
  if p_auth then
    let env = [('i', fun () -> Util.default_image_name base p)] in
    copy_string_with_macros conf env (sou base p.notes)
  else ()
;

value print_occupation conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (capitale (sou base p.occupation)) else ()
;

value print_on_baptism_date conf base env p p_auth =
  if p_auth then
    match Adef.od_of_codate p.baptism with
    [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
    | None -> () ]
  else ()
;

value print_on_birth_date conf base env p p_auth =
  if p_auth then
    match Adef.od_of_codate p.birth with
    [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
    | None -> () ]
  else ()
;

value print_on_burial_date conf base env p p_auth =
  if p_auth then
    match p.burial with
    [ Buried cod ->
        match Adef.od_of_codate cod with
        [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
        | None -> () ]
    | _ -> () ]
  else ()
;

value print_on_cremation_date conf base env p p_auth =
  if p_auth then
    match p.burial with
    [ Cremated cod ->
        match Adef.od_of_codate cod with
        [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
        | None -> () ]
    | _ -> () ]
  else ()
;

value print_on_death_date conf base env p p_auth =
  if p_auth then
    match p.death with
    [ Death _ d ->
        let d = Adef.date_of_cdate d in
        Wserver.wprint "%s" (Date.string_of_ondate conf d)
    | _ -> () ]
  else ()
;

value print_origin_file conf base env =
  if conf.wizard then
    match get_env "fam" env with
    [ Vfam fam _ _ -> Wserver.wprint "%s" (sou base fam.origin_file)
    | _ -> () ]
  else ()
;

value print_parent_age conf base p a p_auth parent =
  match a.parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let pp = poi base (parent cpl) in
      if p_auth && age_autorise conf base pp then
        match (Adef.od_of_codate pp.birth, Adef.od_of_codate p.birth) with
        [ (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
            Date.print_age conf (temps_ecoule d1 d2)
        | _ -> () ]
      else ()
  | None -> () ]
;

value print_prefix_no_templ conf base env =
  let henv =
    List.fold_right
      (fun (k, v) henv -> if k = "templ" then henv else [(k, v) :: henv])
      conf.henv []
  in
  do conf.henv := henv;
      Wserver.wprint "%s" (commd conf);
  return ()
;

value print_public_name conf base env p p_auth =
  Wserver.wprint "%s" (sou base p.public_name)
;

value print_qualifier conf base env p p_auth =
  match (get_env "qualifier" env, p.qualifiers) with
  [ (Vstring nn, _) -> Wserver.wprint "%s" nn
  | (_, [nn :: _]) -> Wserver.wprint "%s" (sou base nn)
  | _ -> () ]
;

value print_referer conf base env =
  Wserver.wprint "%s" (Wserver.extract_param "referer: " '\n' conf.request)
;

value print_related conf base env =
  match get_env "c" env with
  [ Vind c _ _ ->
      do Wserver.wprint "%s" (referenced_person_title_text conf base c);
         Date.afficher_dates_courtes conf base c;
      return ()
  | _ -> () ]
;

value print_related_type conf base env =
  match (get_env "c" env, get_env "rel" env) with
  [ (Vind c _ _, Vrel r) ->
      Wserver.wprint "%s"
        (capitale (rchild_type_text conf r.r_type (index_of_sex c.sex)))
  | _ -> () ]
;

value print_relation_her conf base env =
  match get_env "rel" env with
  [ Vrel {r_moth = Some ip} ->
      let p = poi base ip in
      do Wserver.wprint "%s" (referenced_person_title_text conf base p);
         Date.afficher_dates_courtes conf base p;
      return ()
  | _ -> () ]
;

value print_relation_him conf base env =
  match get_env "rel" env with
  [ Vrel {r_fath = Some ip} ->
      let p = poi base ip in
      do Wserver.wprint "%s" (referenced_person_title_text conf base p);
         Date.afficher_dates_courtes conf base p;
      return ()
  | _ -> () ]
;

value print_relation_type conf base env =
  match get_env "rel" env with
  [ Vrel r ->
      match (r.r_fath, r.r_moth) with
      [ (Some ip, None) ->
           Wserver.wprint "%s" (capitale (relation_type_text conf r.r_type 0))
      | (None, Some ip) ->
           Wserver.wprint "%s" (capitale (relation_type_text conf r.r_type 1))
      | (Some ip1, Some ip2) ->
           Wserver.wprint "%s" (capitale (relation_type_text conf r.r_type 2))
      | _ -> () ]
  | _ -> () ]
;

value print_sosa conf base env a a_auth =
  match get_env "sosa" env with
  [ Vsosa x ->
      match Lazy.force x with
      [ Some (n, p) ->
          Num.print (fun x -> Wserver.wprint "%s" x)
            (transl conf "(thousand separator)") n
      | None -> () ]
  | _ -> () ]
;

value print_sosa_link conf base env a a_auth =
  match get_env "sosa" env with
  [ Vsosa x ->
      match Lazy.force x with
      [ Some (n, p) ->
          Wserver.wprint "m=RL;i1=%d;i2=%d;b1=1;b2=%s"
            (Adef.int_of_iper a.cle_index) (Adef.int_of_iper p.cle_index)
            (Num.to_string n)
      | None -> () ]
  | _ -> () ]
;

value print_source conf base env =
  match get_env "src" env with
  [ Vstring s -> copy_string_with_macros conf [] s
  | _ -> () ]
;

value print_source_type conf base env =
  match get_env "src_typ" env with
  [ Vstring s -> Wserver.wprint "%s" s
  | _ -> () ]
;

value print_surname_alias conf base env =
  match get_env "surname_alias" env with
  [ Vstring s -> Wserver.wprint "%s" s
  | _ -> () ]
;  

value print_surname_key conf base env p p_auth =
  Wserver.wprint "%s" (code_varenv (Name.lower (p_surname base p)))
;

value print_witness_relation conf base env =
  fun
  [ Vfam _ (ip1, ip2) _ ->
      Wserver.wprint
        (fcapitale (ftransl conf "witness at marriage of %s and %s"))
        (referenced_person_title_text conf base (poi base ip1))
        (referenced_person_title_text conf base (poi base ip2))
  | _ -> () ]
;

value eval_int_env var env =
  match get_env var env with
  [ Vint x -> string_of_int x
  | _ -> "" ]
;

value try_eval_gen_variable conf base env =
  fun
  [ "child_cnt" -> eval_int_env "child_cnt" env
  | "family_cnt" -> eval_int_env "family_cnt" env
  | "nb_children" ->
      match get_env "fam" env with
      [ Vfam _ _ des -> string_of_int (Array.length des.children)
      | _ -> "" ]
  | "nb_families" ->
      match get_env "child" env with
      [ Vind _ _ u -> string_of_int (Array.length u.family)
      | _ ->
          match get_env "p" env with
          [ Vind _ _ u -> string_of_int (Array.length u.family)
          | _ -> "" ] ]
(**)
  | "count" ->
      do obsolete "count" "child_cnt"; return eval_int_env "child_cnt" env
  | "length" ->
      do obsolete "length" "nb_children"; return
      match get_env "fam" env with
      [ Vfam _ _ des -> string_of_int (Array.length des.children)
      | _ -> "" ]
(**)
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> quote_escaped vv
        | _ -> "" ]
      else raise Not_found ]
;

value print_simple_variable conf base env (p, a, u, p_auth) efam =
  fun
  [ "access" -> Wserver.wprint "%s" (acces conf base p)
  | "age" -> print_age conf base env p p_auth
  | "alias" -> print_alias conf base env
  | "baptism_place" -> print_baptism_place conf base env p p_auth
  | "birth_place" -> print_birth_place conf base env p p_auth
  | "border" -> Wserver.wprint "%d" conf.border
  | "burial_place" -> print_burial_place conf base env p p_auth
  | "child_name" ->
      let force_surname =
        match a.parents with
        [ None -> False
        | Some ifam ->
            p_surname base (poi base (coi base ifam).father) <>
            p_surname base p ]
      in
      Wserver.wprint "%s"
        (if force_surname then person_text conf base p
         else person_text_without_surname conf base p)
  | "cremation_place" -> print_burial_place conf base env p p_auth
  | "comment" -> print_comment conf base env p p_auth efam
  | "consanguinity" -> print_consanguinity conf base env a p_auth
  | "death_age" -> print_death_age conf base env p p_auth
  | "death_place" -> print_death_place conf base env p p_auth
  | "died" -> print_died conf base env p p_auth
  | "divorce_date" -> print_divorce_date conf base env p p_auth efam
  | "dates" -> if p_auth then Date.afficher_dates_courtes conf base p else ()
  | "fam_access" ->
      match efam with
      [ Vfam fam _ _ ->
          Wserver.wprint "i=%d;ip=%d" (Adef.int_of_ifam fam.fam_index)
            (Adef.int_of_iper p.cle_index)
      | _ -> () ]
  | "father_age_at_birth" ->
      print_parent_age conf base p a p_auth (fun cpl -> cpl.father)
  | "first_name" -> Wserver.wprint "%s" (p_first_name base p)
  | "first_name_alias" -> print_first_name_alias conf base env
  | "first_name_key" -> print_first_name_key conf base env p p_auth
  | "image_size" -> print_image_size conf base env p p_auth
  | "image_url" -> print_image_url conf base env p p_auth
  | "ind_access" -> Wserver.wprint "i=%d" (Adef.int_of_iper p.cle_index)
  | "married_to" -> print_married_to conf base env p p_auth efam
  | "mother_age_at_birth" ->
      print_parent_age conf base p a p_auth (fun cpl -> cpl.mother)
  | "nobility_title" -> print_nobility_title conf base env p p_auth
  | "nobility_titles" -> print_nobility_titles conf base env p p_auth
  | "notes" -> print_notes conf base env p p_auth
  | "occupation" -> print_occupation conf base env p p_auth
  | "on_baptism_date" -> print_on_baptism_date conf base env p p_auth
  | "on_birth_date" -> print_on_birth_date conf base env p p_auth
  | "on_burial_date" -> print_on_burial_date conf base env p p_auth
  | "on_cremation_date" -> print_on_cremation_date conf base env p p_auth
  | "on_death_date" -> print_on_death_date conf base env p p_auth
  | "origin_file" -> print_origin_file conf base env
  | "prefix_no_templ" -> print_prefix_no_templ conf base env
  | "public_name" -> print_public_name conf base env p p_auth
  | "qualifier" -> print_qualifier conf base env p p_auth
  | "referer" -> print_referer conf base env
  | "related" -> print_related conf base env
  | "relation_her" -> print_relation_her conf base env
  | "relation_him" -> print_relation_him conf base env
  | "relation_type" -> print_relation_type conf base env
  | "related_type" -> print_related_type conf base env
  | "sosa" -> print_sosa conf base env p p_auth
  | "sosa_link" -> print_sosa_link conf base env p p_auth
  | "source_type" -> print_source_type conf base env
  | "source" -> print_source conf base env
  | "surname" -> Wserver.wprint "%s" (p_surname base p)
  | "surname_alias" -> print_surname_alias conf base env
  | "surname_key" -> print_surname_key conf base env p p_auth
  | "title" -> Wserver.wprint "%s" (person_title conf base p)
  | "witness_relation" -> print_witness_relation conf base env efam
  | s -> Wserver.wprint "%s" (try_eval_gen_variable conf base env s) ]
;

value simple_person_text conf base p p_auth =
  if p_auth then
    match main_title base p with
    [ Some t -> titled_person_text conf base p t
    | None -> person_text conf base p ]
  else person_text conf base p
;

value print_simple_person_text conf base (p, _, _, p_auth) =
  Wserver.wprint "%s" (simple_person_text conf base p p_auth)
;

value print_variable conf base env sl =
  match eval_variable conf base env sl with
  [ Some (ep, efam, "") -> print_simple_person_text conf base ep
  | Some (ep, efam, s) ->
      try print_simple_variable conf base env ep efam s with
      [ Not_found -> Templ.print_variable conf base s ]
  | None ->
      do list_iter_first
           (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
           sl;
         Wserver.wprint "???";
      return () ]
;

value is_restricted conf base p =
  match base.func.is_restricted p.cle_index with
  [ Left x -> x
  | Right False -> False
  | Right True ->
      match Adef.od_of_codate p.birth with
      [ Some (Dgreg d _) -> (temps_ecoule d conf.today).year <= 100
      | _ -> True ] ]
;

value eval_simple_bool_variable conf base env (p, a, u, p_auth) efam =
  fun
  [ "are_divorced" ->
      match efam with
      [ Vfam fam cpl _ ->
          match fam.divorce with
          [ Divorced d -> True
          | _ -> False ]
      | _ -> False ]
  | "are_separated" ->
      match efam with
      [ Vfam fam cpl _ -> fam.divorce = Separated
      | _ -> False ]
  | "birthday" ->
      if p_auth then
        match Adef.od_of_codate p.birth with
        [ Some (Dgreg d _) ->
            if d.prec = Sure && p.death = NotDead then
              d.day = conf.today.day && d.month = conf.today.month &&
              d.year < conf.today.year
            || not (leap_year conf.today.year) && d.day = 29 &&
              d.month = 2 && conf.today.day = 1 &&
              conf.today.month = 3
            else False
        | _ -> False ]
      else False
  | "cancel_links" -> conf.cancel_links
  | "computable_age" ->
      if p_auth then
        match (Adef.od_of_codate p.birth, p.death) with
        [ (Some (Dgreg d _), NotDead) ->
            not (d.day == 0 && d.month == 0 && d.prec <> Sure)
        | _ -> False ]
      else False
  | "computable_death_age" ->
      if p_auth then
        match Date.get_birth_death_date p with
        [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
           Some (Dgreg ({prec = Sure | About | Maybe} as d2) _),
           approx) when d1 <> d2 ->
            let a = temps_ecoule d1 d2 in
            a.year > 0 ||
            a.year = 0 && (a.month > 0 || a.month = 0 && (a.day > 0))
        | _ -> False ]
      else False
  | "has_aliases" -> p.aliases <> []
  | "has_baptism_date" -> p_auth && p.baptism <> Adef.codate_None
  | "has_baptism_place" -> p_auth && sou base p.baptism_place <> ""
  | "has_birth_date" -> p_auth && p.birth <> Adef.codate_None
  | "has_birth_place" -> p_auth && sou base p.birth_place <> ""
  | "has_burial_date" ->
      if p_auth then
        match p.burial with
        [ Buried cod -> Adef.od_of_codate cod <> None
        | _ -> False ] 
      else False
  | "has_burial_place" -> p_auth && sou base p.burial_place <> ""
  | "has_cremation_date" ->
      if p_auth then
        match p.burial with
        [ Cremated cod -> Adef.od_of_codate cod <> None
        | _ -> False ] 
      else False
  | "has_children" ->
      match efam with
      [ Vfam _ _ des -> Array.length des.children > 0
      | _ ->
          List.exists
            (fun ifam ->
               let des = doi base ifam in
               Array.length des.children > 0)
            (Array.to_list u.family) ]
  | "has_comment" ->
      match efam with
      [ Vfam fam _ _ -> p_auth && sou base fam.comment <> ""
      | _ -> False ]
  | "has_consanguinity" ->
      p_auth && a.consang != Adef.fix (-1) && a.consang != Adef.fix 0
  | "has_cremation_place" -> p_auth && sou base p.burial_place <> ""
  | "has_death_date" ->
      match p.death with
      [ Death _ _ -> p_auth
      | _ -> False ]
  | "has_death_place" -> p_auth && sou base p.death_place <> ""
  | "has_families" -> Array.length u.family > 0
  | "has_first_names_aliases" -> p.first_names_aliases <> []
  | "has_image" ->
      match get_env "image" env with
      [ Vimage x ->
          match Lazy.force x with
          [ Some (_, Some (Some _)) | Some (_, None) -> True
          | _ -> False ]
      | _ -> False ]
  | "has_nephews_or_nieces" -> has_nephews_or_nieces base p
  | "has_nobility_titles" -> p_auth && p.titles <> []
  | "has_notes" -> p_auth && sou base p.notes <> ""
  | "has_occupation" -> p_auth && sou base p.occupation <> ""
  | "has_parents" -> a.parents <> None
  | "has_public_name" -> sou base p.public_name <> ""
  | "has_qualifiers" -> p.qualifiers <> []
  | "has_referer" -> Wserver.extract_param "referer: " '\n' conf.request <> ""
  | "has_relation_her" ->
      match get_env "rel" env with
      [ Vrel {r_moth = Some _} -> True
      | _ -> False ]
  | "has_relation_him" ->
      match get_env "rel" env with
      [ Vrel {r_fath = Some _} -> True
      | _ -> False ]
  | "has_relations" -> p_auth && (p.rparents <> [] || p.related <> [])
  | "has_siblings" ->
      match a.parents with
      [ Some ifam -> Array.length (doi base ifam).children > 1
      | None -> False ]
  | "has_sosa" ->
      match get_env "sosa" env with
      [ Vsosa x -> Lazy.force x <> None
      | _ -> False ]
  | "has_sources" ->
      if sou base p.psources <> "" then True
      else if
        p_auth &&
        (sou base p.birth_src <> "" || sou base p.baptism_src <> "" ||
         sou base p.death_src <> "" || sou base p.burial_src <> "") then
        True
      else
        List.exists
          (fun ifam ->
             let fam = foi base ifam in
             p_auth && sou base fam.marriage_src <> "" ||
             sou base fam.fsources <> "")
          (Array.to_list u.family)
  | "has_surnames_aliases" -> p.surnames_aliases <> []
  | "has_witnesses" ->
      match efam with
      [ Vfam fam _ _ -> Array.length fam.witnesses > 0
      | _ -> False ]
  | "is_buried" ->
      match p.burial with
      [ Buried _ -> p_auth
      | _ -> False ]
  | "is_cremated" ->
      match p.burial with
      [ Cremated _ -> p_auth
      | _ -> False ]
  | "is_dead" ->
      match p.death with
      [ Death _ _ | DeadYoung | DeadDontKnowWhen -> p_auth
      | _ -> False ]
  | "is_female" -> p.sex = Female
  | "is_first" ->
       match get_env "first" env with
       [ Vbool x -> x
       | _ -> False ]
  | "is_male" -> p.sex = Male
  | "is_sibling_after" -> get_env "pos" env = Vstring "next"
  | "is_sibling_before" -> get_env "pos" env = Vstring "prev"
  | "is_private" -> p.access = Private
  | "is_public" -> p.access = Public
  | "is_self" -> get_env "pos" env = Vstring "self"
  | "wizard" -> conf.wizard
  | v -> do Wserver.wprint ">%%%s???" v; return False ]
;

value eval_bool_variable conf base env sl =
  match eval_variable conf base env sl with
  [ Some (ep, efam, "") ->
      do list_iter_first
           (fun first s ->
              Wserver.wprint "%s%s" (if first then "" else ".") s)
           sl;
         Wserver.wprint "???";
      return False
  | Some (ep, efam, s) -> eval_simple_bool_variable conf base env ep efam s
  | None -> False ]
;

value split_at_coloncolon s =
  loop 0 where rec loop i =
    if i >= String.length s - 1 then None
    else
      match (s.[i], s.[i+1]) with
      [ (':', ':') ->
          let s1 = String.sub s 0 i in
          let s2 = String.sub s (i + 2) (String.length s - i - 2) in
          Some (s1, s2)
      | _ -> loop (i + 1) ]
;

value print_transl conf base env upp s c =
  let r =
    match c with
    [ '0'..'9' ->
        let n = Char.code c - Char.code '0' in
        match split_at_coloncolon s with
        [ None -> nominative (Util.transl_nth conf s n)
        | Some (s1, s2) ->
            Util.transl_decline conf s1 (Util.transl_nth conf s2 n) ]
    | 'n' ->
        let n =
          match get_env "p" env with
          [ Vind p _ _ -> 1 - index_of_sex p.sex
          | _ -> 2 ]
        in
        Util.transl_nth conf s n
    | 's' ->
        let n =
          match get_env "child" env with
          [ Vind p _ _ -> index_of_sex p.sex
          | _ ->
              match get_env "p" env with
              [ Vind p _ _ -> index_of_sex p.sex
              | _ -> 2 ] ]
        in
        Util.transl_nth conf s n
    | 'w' ->
        let n =
          match get_env "fam" env with
          [ Vfam fam _ _ -> if Array.length fam.witnesses = 1 then 0 else 1
          | _ -> 0 ]
        in
        Util.transl_nth conf s n
    | _ -> nominative (Util.transl conf s) ^ String.make 1 c ]
  in
  Wserver.wprint "%s" (if upp then capitale r else r)
;

value print_wid_hei conf base env fname =
  match image_size (image_file_name fname) with
  [ Some (wid, hei) -> Wserver.wprint " width=%d height=%d" wid hei
  | None -> () ]
;

value eval_bool_value conf base env =
  let rec bool_eval =
    fun
    [ Eor e1 e2 -> bool_eval e1 || bool_eval e2
    | Eand e1 e2 -> bool_eval e1 && bool_eval e2
    | Eop op e1 e2 ->
        match op with
        [ "=" -> string_eval e1 = string_eval e2
        | "!=" -> string_eval e1 <> string_eval e2
        | _ -> do Wserver.wprint "op %s???" op; return False ]
    | Enot e -> not (bool_eval e)
    | Evar s sl -> eval_bool_variable conf base env [s :: sl]
    | Estr s -> do Wserver.wprint "\"%s\"???" s; return False
    | Eint s -> do Wserver.wprint "\"%s\"???" s; return False
    | Etransl _ s _ -> do Wserver.wprint "[%s]???" s; return False ]
  and string_eval =
    fun
    [ Estr s -> s
    | Eint s -> s
    | Evar s sl ->
        try try_eval_gen_variable conf base env s with
        [ Not_found -> do Wserver.wprint ">%%%s???" s; return "" ]
    | x -> do Wserver.wprint "val???"; return "" ]
  in
  bool_eval
;

value make_ep base ip =
  let p = poi base ip in
  let a = aoi base ip in
  let u = uoi base ip in
  Vind p a u
;

value rec eval_ast conf base env =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n -> print_transl conf base env upp s n
  | Avar s sl -> print_variable conf base env [s :: sl]
  | Awid_hei s -> print_wid_hei conf base env s
  | Aif e alt ale -> eval_if conf base env e alt ale
  | Aforeach s sl al -> eval_foreach conf base env s sl al
  | Adefine f x al alk -> ()
  | Aapply f v -> () ]
and eval_if conf base env e alt ale =
  let al = if eval_bool_value conf base env e then alt else ale in
  List.iter (eval_ast conf base env) al
and eval_foreach conf base env s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in
    (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env sl with
  [ Some (ep, efam, "") -> eval_simple_foreach conf base env al ep efam s
  | Some (ep, efam, _) ->
      do Wserver.wprint "foreach ";
         List.iter (fun s -> Wserver.wprint "%s." s) sl;
         Wserver.wprint "%s???" s;
      return ()
  | None -> () ]
and eval_simple_foreach conf base env al ep efam =
  fun
  [ "alias" -> eval_foreach_alias conf base env al ep
  | "child" -> eval_foreach_child conf base env al efam
  | "family" -> eval_foreach_family conf base env al ep
  | "first_name_alias" -> eval_foreach_first_name_alias conf base env al ep
  | "nobility_title" -> eval_foreach_nobility_title conf base env al ep
  | "qualifier" -> eval_foreach_qualifier conf base env al ep
  | "related" -> eval_foreach_related conf base env al ep
  | "relation" -> eval_foreach_relation conf base env al ep
  | "source" -> eval_foreach_source conf base env al ep
  | "surname_alias" -> eval_foreach_surname_alias conf base env al ep
  | "witness" -> eval_foreach_witness conf base env al efam
  | "witness_relation" -> eval_foreach_witness_relation conf base env al ep
  | s -> Wserver.wprint "foreach %s???" s ]
and eval_foreach_alias conf base env al (p, _, _, p_auth) =
  List.iter
    (fun a ->
       let env = [("alias", Vstring (sou base a)) :: env] in
       List.iter (eval_ast conf base env) al)
    p.aliases
and eval_foreach_child conf base env al =
  fun
  [ Vfam _ _ des ->
      let auth =
        List.for_all
          (fun ip -> age_autorise conf base (poi base ip))
          (Array.to_list des.children)
      in
      let env = [("auth", Vbool auth) :: env] in
      let n =
        let p =
          match get_env "p" env with
          [ Vind p _ _ -> p
          | _ -> assert False ]
        in
        loop 0 where rec loop i =
          if i = Array.length des.children then -2
          else if des.children.(i) = p.cle_index then i else loop (i + 1)
      in
      Array.iteri
        (fun i ip ->
           let p = poi base ip in
           let a = aoi base ip in
           let u = uoi base ip in
           let env = [("child", Vind p a u) :: env] in
           let env = [("child_cnt", Vint (i + 1)) :: env] in
           let env =
             if i = n - 1 then [("pos", Vstring "prev") :: env]
             else if i = n then [("pos", Vstring "self") :: env]
             else if i = n + 1 then [("pos", Vstring "next") :: env]
             else env
           in
           List.iter (eval_ast conf base env) al)
        des.children
  | _ -> () ]
and eval_foreach_family conf base env al (p, _, u, _) =
  Array.iteri
    (fun i ifam ->
       let fam = foi base ifam in
       let cpl = coi base ifam in
       let des = doi base ifam in
       let cpl = (p.cle_index, spouse p.cle_index cpl) in
       let env = [("fam", Vfam fam cpl des) :: env] in
       let env = [("family_cnt", Vint (i + 1)) :: env] in
       List.iter (eval_ast conf base env) al)
    u.family
and eval_foreach_first_name_alias conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun s ->
         let env = [("first_name_alias", Vstring (sou base s)) :: env] in
         List.iter (eval_ast conf base env) al)
      p.first_names_aliases
  else ()
and eval_foreach_nobility_title conf base env al (p, _, _, p_auth) =
  if p_auth then
    let titles = nobility_titles_list conf p in
    list_iter_first
      (fun first x ->
         let env = [("nobility_title", Vtitle x) :: env] in
         let env = [("first", Vbool first) :: env] in
         List.iter (eval_ast conf base env) al)
      titles
  else ()
and eval_foreach_qualifier conf base env al (p, _, _, _) =
  list_iter_first
    (fun first nn ->
       let env = [("qualifier", Vstring (sou base nn)) :: env] in
       let env = [("first", Vbool first) :: env] in
       List.iter (eval_ast conf base env) al)
    p.qualifiers
and eval_foreach_relation conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun r ->
         let env = [("rel", Vrel r) :: env] in
         List.iter (eval_ast conf base env) al)
      p.rparents
  else ()
and eval_foreach_related conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun ic ->
         let c = poi base ic in
         let a = aoi base ic in
         let u = uoi base ic in
         let env = [("c", Vind c a u) :: env] in
         List.iter
           (fun r ->
              do match r.r_fath with
                 [ Some ip ->
                     if ip = p.cle_index then
                       let env = [("rel", Vrel r) :: env] in
                       List.iter (eval_ast conf base env) al
                     else ()
                 | None -> () ];
                 match r.r_moth with
                 [ Some ip ->
                     if ip = p.cle_index then
                       let env = [("rel", Vrel r) :: env] in
                       List.iter (eval_ast conf base env) al
                     else ()
                 | None -> () ];
              return ())
           c.rparents)
      p.related
  else ()
and eval_foreach_source conf base env al (p, _, u, p_auth) =
  let print_src src_typ src =
    let s = sou base src in
    if s = "" then ()
    else
      let env =
        [("src_typ", Vstring src_typ); ("src", Vstring s) :: env]
      in
      List.iter (eval_ast conf base env) al
  in
  do print_src (nominative (transl_nth conf "person/persons" 0)) p.psources;
     if p_auth then
       do print_src (transl_nth conf "birth" 0) p.birth_src;
          print_src (transl_nth conf "baptism" 0) p.baptism_src;
          print_src (transl_nth conf "death" 0) p.death_src;
          print_src (transl_nth conf "burial" 0) p.burial_src;
       return ()
     else ();
     for i = 0 to Array.length u.family - 1 do
       let fam = foi base u.family.(i) in
       let lab =
         if Array.length u.family == 1 then ""
         else " " ^ string_of_int (i + 1)
       in
       do if p_auth then
            let src_typ =
              nominative (transl_nth conf "marriage/marriages" 0)
            in
            print_src (src_typ ^ lab) fam.marriage_src
          else ();
          let src_typ = nominative (transl_nth conf "family/families" 0) in
          print_src (src_typ ^ lab) fam.fsources;
       return ();
     done;
  return ()
and eval_foreach_surname_alias conf base env al (p, _, _, _) =
  List.iter
    (fun s ->
       let env = [("surname_alias", Vstring (sou base s)) :: env] in
       List.iter (eval_ast conf base env) al)
    p.surnames_aliases
and eval_foreach_witness conf base env al =
  fun
  [ Vfam fam _ _ ->
      list_iter_first
        (fun first ip ->
           let p = poi base ip in
           let a = aoi base ip in
           let u = uoi base ip in
           let env = [("witness", Vind p a u) :: env] in
           let env = [("first", Vbool first) :: env] in
           List.iter (eval_ast conf base env) al)
        (Array.to_list fam.witnesses)
  | _ -> () ]
and eval_foreach_witness_relation conf base env al (p, _, _, _) =
  List.iter
    (fun ic ->
       let c = poi base ic in
       if c.sex = Male then
         Array.iter
           (fun ifam ->
              let fam = foi base ifam in
              if array_memq p.cle_index fam.witnesses then
                let cpl = coi base ifam in
                let des = doi base ifam in
                let cpl = (cpl.father, cpl.mother) in
                let env = [("fam", Vfam fam cpl des) :: env] in
                List.iter (eval_ast conf base env) al
              else ())
           (uoi base ic).family
       else ())
    p.related
;

value interp_templ conf base p astl =
  let a = aoi base p.cle_index in
  let u = uoi base p.cle_index in
  let env =
    let env = [] in
    let env =
      let v = lazy (find_sosa conf base p) in
      [("sosa", Vsosa v) :: env]
    in
    let env =
      let v =
        lazy
          (image_and_size conf base p
             (limited_image_size max_im_wid max_im_wid))
      in
      [("image", Vimage v) :: env]
    in
    let env = [("p_auth", Vbool (age_autorise conf base p)) :: env] in
    let env = [("p", Vind p a u) :: env] in
    env
  in
  List.iter (eval_ast conf base env) astl
;

(* Main *)

value print_ok conf base p =
(**)
  if conf.wizard || conf.friend || not (is_restricted conf base p) then
(**)
    let astl = Templ.input conf base "perso" in
    do html conf; interp_templ conf base p astl; return ()
(**)
  else
    do Util.header conf (fun _ -> Wserver.wprint "Restricted");
       Util.print_link_to_welcome conf True;
       Util.trailer conf;
    return ()
(**)
;

value print conf base p =
  let passwd =
    if conf.wizard || conf.friend then None
    else
      let src =
        match (aoi base p.cle_index).parents with
        [ Some ifam -> sou base (foi base ifam).origin_file
        | None -> "" ]
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.base_env)
      with [ Not_found -> None ]
  in
  match passwd with
  [ Some (src, passwd) when passwd <> conf.passwd -> Util.unauthorized conf src
  | _ -> print_ok conf base p ]
;
