(* camlp4r *)
(* $Id: perso.ml,v 4.60 2004-12-14 09:30:15 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Gutil;
open Util;
open Config;
open TemplAst;

value max_im_wid = 240;
value max_im_hei = 240;
value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

value has_children base u =
  List.exists
    (fun ifam -> let des = doi base ifam in Array.length des.children > 0)
    (Array.to_list u.family)
;

value string_of_marriage_text conf base fam =
  let marriage = Adef.od_of_codate fam.marriage in
  let marriage_place = sou base fam.marriage_place in
  let s =
    match marriage with
    [ Some d -> " " ^ Date.string_of_ondate conf d
    | _ -> "" ]
  in
  match marriage_place with
  [ "" -> s
  | _ -> s ^ ", " ^ string_with_macros conf False [] marriage_place ^ "," ]
;

value string_of_title conf base and_txt p (nth, name, title, places, dates) =
  let href =
    "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
      code_varenv (sou base (List.hd places))
  in
  let (tit, est) = (sou base title, sou base (List.hd places)) in
  let s = tit ^ " " ^ est in
  let b = Buffer.create 50 in
  do {
    Buffer.add_string b (geneweb_link conf href s);
    let rec loop places =
      do {
        match places with
        [ [] -> ()
        | [_] -> Printf.bprintf b "\n%s " and_txt
        | _ -> Buffer.add_string b ",\n" ];
        match places with
        [ [place :: places] ->
            let href =
              "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
                code_varenv (sou base place)
            in
            let est = sou base place in
            do {
              Buffer.add_string b (geneweb_link conf href est);
              loop places
            }
        | _ -> () ]
      }
    in
    loop (List.tl places);
    let paren =
      match (nth, dates, name) with
      [ (n, _, _) when n > 0 -> True
      | (_, _, Tname _) -> True
      | (_, [(Some _, _) :: _], _) -> authorized_age conf base p
      | _ -> False ]
    in
    if paren then Buffer.add_string b "\n(" else ();
    let first =
      if nth > 0 then do {
        Buffer.add_string b
          (if nth >= 100 then string_of_int nth
           else transl_nth conf "nth" nth);
        False
      }
      else True
    in
    let first =
      match name with
      [ Tname n ->
          do {
            if not first then Buffer.add_string b " ," else ();
            Buffer.add_string b (sou base n);
            False
          }
      | _ -> first ]
    in
    if authorized_age conf base p && dates <> [(None, None)] then
      let _ =
        List.fold_left
          (fun first (date_start, date_end) ->
             do {
               if not first then Buffer.add_string b ",\n" else ();
               match date_start with
               [ Some d -> Buffer.add_string b (Date.string_of_date conf d)
               | None -> () ];
               match date_end with
               [ Some (Dgreg d _) ->
                   if d.month <> 0 then Buffer.add_string b " - "
                   else Buffer.add_string b "-"
               | _ -> () ];
               match date_end with
               [ Some d -> Buffer.add_string b (Date.string_of_date conf d)
               | None -> () ];
               False
             })
          first dates
      in
      ()
    else ();
    if paren then Buffer.add_string b ")" else ();
    Buffer.contents b
  }
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
             not conf.is_rtl && nth = t.t_nth && name_equiv name t.t_name &&
             title = t.t_ident && place = t.t_place ->
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
           not conf.is_rtl && nth = t_nth && name_equiv name t_name &&
           title = t_ident && dates = t_dates ->
           [(nth, name, title, [t_place :: places], dates) :: rl]
       | _ -> [(t_nth, t_name, t_ident, [t_place], t_dates) :: l] ])
    titles []
;

(* obsolete; should be removed one day *)

value string_of_titles conf base cap and_txt p =
  let titles = nobility_titles_list conf p in
  List.fold_left
    (fun s t ->
       s ^ (if s = "" then "" else ",") ^ "\n" ^
       string_of_title conf base and_txt p t)
    "" titles
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
        else do {
          mark.(Adef.int_of_iper ip) := True;
          if tstab.(Adef.int_of_iper a.cle_index) <=
               tstab.(Adef.int_of_iper ip) then
            gene_find zil
          else
            let asc = aget conf base ip in
            match parents asc with
            [ Some ifam ->
                let cpl = coi base ifam in
                let z = Num.twice z in
                match gene_find zil with
                [ Left zil ->
                    Left [(z, father cpl); (Num.inc z 1, (mother cpl)) :: zil]
                | Right z -> Right z ]
            | None -> gene_find zil ]
        } ]
  in
  let rec find zil =
    match gene_find zil with
    [ Left [] -> None
    | Left zil -> find zil
    | Right z -> Some (z, p) ]
  in
  find [(Num.one, p.cle_index)]
;
(* Male version
value find_sosa_aux conf base a p =
  let mark = Array.create base.data.persons.len False in
  let rec find z ip =
    if ip = a.cle_index then Some z
    else if mark.(Adef.int_of_iper ip) then None
    else do {
      mark.(Adef.int_of_iper ip) := True;
      let asc = aget conf base ip in
      match asc.parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let z = Num.twice z in
          match find z (father cpl) with
          [ Some z -> Some z
          | None -> find (Num.inc z 1) (mother cpl) ]
      | None -> None ]
    }
  in
  find Num.one p.cle_index
;
*)

value find_sosa conf base a =
  match Util.find_sosa_ref conf base with
  [ Some p ->
      if a.cle_index = p.cle_index then Some (Num.one, p)
      else
        let u = uget conf base a.cle_index in
        if has_children base u then find_sosa_aux conf base a p else None
  | None -> None ]
;

(* Interpretation of template file 'perso.txt' *)

type env =
  [ Vind of person and ascend and union
  | Vfam of family and (iper * iper) and descend
  | Vrel of relation
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vsosa of option (Num.t * person)
  | Vimage of option (bool * string * option (int * int))
  | Vtitle of title_item
  | Vfun of list string and list ast
  | Vnone ]
and title_item =
  (int * gen_title_name istr * istr * list istr *
   list (option date * option date))
;

type variable_value =
  [ VVsome of
      (list (string * env) * (person * ascend * union * bool) * env * string)
  | VVcvar of string
  | VVnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value rec eval_variable conf base env sl =
  let ep =
    match (get_env "p" env, get_env "p_auth" env) with
    [ (Vind p a u, Vbool p_auth) -> (p, a, u, p_auth)
    | _ -> assert False ]
  in
  let efam = get_env "fam" env in
  let make_ep ip =
    let p = pget conf base ip in
    let a = aget conf base ip in
    let u = uget conf base ip in
    let p_auth = authorized_age conf base p in (p, a, u, p_auth)
  in
  let rec loop (p, a, u, p_auth) efam =
    fun
    [ ["child" :: sl] ->
        match get_env "child" env with
        [ Vind p a u ->
            let auth =
              match get_env "auth" env with
              [ Vbool True -> authorized_age conf base p
              | _ -> False ]
            in
            let ep = (p, a, u, auth) in loop ep Vnone sl
        | _ -> VVnone ]
    | ["parent" :: sl] ->
        match get_env "parent" env with
        [ Vind p a u ->
            let ep = (p, a, u, authorized_age conf base p ) in loop ep efam sl
        | _ -> VVnone ]
    | ["father" :: sl] ->
        match parents a with
        [ Some ifam ->
            let cpl = coi base ifam in
            let ep = make_ep (father cpl) in
            let cpl = ((father cpl), (mother cpl)) in
            let efam = Vfam (foi base ifam) cpl (doi base ifam) in
            loop ep efam sl
        | None -> VVnone ]
    | ["mother" :: sl] ->
        match parents a with
        [ Some ifam ->
            let cpl = coi base ifam in
            let ep = make_ep (mother cpl) in
            let cpl = ((mother cpl), (father cpl)) in
            let efam = Vfam (foi base ifam) cpl (doi base ifam) in
            loop ep efam sl
        | None -> VVnone ]
    | ["related" :: sl] ->
        match get_env "c" env with
        [ Vind p a u ->
            let ep = (p, a, u, authorized_age conf base p) in loop ep efam sl
        | _ -> VVnone ]
    | ["relation_her" :: sl] ->
        match get_env "rel" env with
        [ Vrel {r_moth = Some ip} -> let ep = make_ep ip in loop ep efam sl
        | _ -> VVnone ]
    | ["relation_him" :: sl] ->
        match get_env "rel" env with
        [ Vrel {r_fath = Some ip} -> let ep = make_ep ip in loop ep efam sl
        | _ -> VVnone ]
    | ["self" :: sl] -> loop (p, a, u, p_auth) efam sl
    | ["spouse" :: sl] ->
        match efam with
        [ Vfam fam (_, ip) _ -> let ep = make_ep ip in loop ep efam sl
        | _ -> VVnone ]
    | ["witness" :: sl] ->
        match get_env "witness" env with
        [ Vind p a u ->
            let ep = (p, a, u, authorized_age conf base p) in loop ep efam sl
        | _ -> VVnone ]
    | ["enclosing" :: sl] ->
        let rec loop =
          fun
          [ [("#loop", _) :: env] -> eval_variable conf base env sl
          | [_ :: env] -> loop env
          | [] -> VVnone ]
        in
        loop env
    | [] -> VVsome (env, (p, a, u, p_auth), efam, "")
    | [s] ->
        let v = extract_var "cvar_" s in
        if v <> "" then VVcvar v else VVsome (env, (p, a, u, p_auth), efam, s)
    | _ -> VVnone ]
  in
  loop ep efam sl
;

value eval_base_env_variable conf v =
  try List.assoc v conf.base_env with [ Not_found -> "" ]
;

value simple_person_text conf base p p_auth =
  if p_auth then
    match main_title base p with
    [ Some t -> titled_person_text conf base p t
    | None -> person_text conf base p ]
  else if conf.hide_names then "x x"
  else person_text conf base p
;

value string_of_age conf base env p p_auth =
  if p_auth then
    match (Adef.od_of_codate p.birth, p.death) with
    [ (Some (Dgreg d _), NotDead) ->
        let a = time_gone_by d conf.today in
        Date.string_of_age conf a
    | _ -> "" ]
  else ""
;

value string_of_alias conf base env =
  match get_env "alias" env with
  [ Vstring s -> s
  | _ -> "" ]
;

value string_of_place conf base istr =
  string_with_macros conf False [] (sou base istr)
;

value string_of_baptism_place conf base env p p_auth =
  if p_auth then string_of_place conf base p.baptism_place else ""
;

value string_of_birth_place conf base env p p_auth =
  if p_auth then string_of_place conf base p.birth_place else ""
;

value string_of_burial_place conf base env p p_auth =
  if p_auth then string_of_place conf base p.burial_place else ""
;

value string_of_comment conf base env p p_auth =
  fun
  [ Vfam fam _ _ ->
      if p_auth then string_with_macros conf False [] (sou base fam.comment)
      else ""
  | _ -> "" ]
;

value string_of_consanguinity conf base env a p_auth =
  if p_auth then
    string_of_decimal_num conf
      (round_2_dec (Adef.float_of_fix (consang a) *. 100.0)) ^
    "%"
  else ""
;

value string_of_death_age conf base env p p_auth =
  if p_auth then
    match Date.get_birth_death_date p with
    [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
       Some (Dgreg ({prec = Sure | About | Maybe} as d2) _), approx)
      when d1 <> d2 ->
        let a = time_gone_by d1 d2 in
        let s =
          if not approx && d1.prec = Sure && d2.prec = Sure then ""
          else transl_decline conf "possibly (date)" "" ^ " "
        in
        s ^ Date.string_of_age conf a
    | _ -> "" ]
  else ""
;

value string_of_death_place conf base env p p_auth =
  if p_auth then string_of_place conf base p.death_place else ""
;

value string_of_died conf base env p p_auth =
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
        capitale dr_w
    | DeadYoung ->
        capitale (transl_nth conf "died young" is)
    | DeadDontKnowWhen ->
        capitale (transl_nth conf "died" is)
    | _ -> "" ]
  else ""
;

value string_of_divorce_date conf base env p p_auth =
  fun
  [ Vfam fam (_, isp) _ ->
      match fam.divorce with
      [ Divorced d ->
          let d = Adef.od_of_codate d in
          let auth =
            let spouse = pget conf base isp in
            p_auth && authorized_age conf base spouse
          in
          match d with
          [ Some d when auth ->
              " <em>" ^ Date.string_of_ondate conf d ^ "</em>"
          | _ -> "" ]
      | _ -> "" ]
  | _ -> "" ]
;

value string_of_first_name_alias conf base env =
  match get_env "first_name_alias" env with
  [ Vstring s -> s
  | _ -> "" ]
;  

value string_of_image_size conf base env p p_auth =
  if p_auth then
    match get_env "image" env with
    [ Vimage x ->
        match x with
        [ Some (_, _, Some (width, height)) ->
            Format.sprintf " width=%d height=%d" width height
        | Some (_, link, None) -> Format.sprintf " height=%d" max_im_hei
        | None -> "" ]
    | _ -> "" ]
  else ""
;

value string_of_image_url conf base env p p_auth html =
  if p_auth then
    match get_env "image" env with
    [ Vimage x ->
        match x with
        [ Some (True, fname, _) ->
            let s = Unix.stat fname in
            let b = acces conf base p in
            let k = default_image_name base p in
            Format.sprintf "%sm=IM%s;d=%d;%s;k=/%s" (commd conf)
              (if html then "H" else "")
              (int_of_float
                 (mod_float s.Unix.st_mtime (float_of_int max_int)))
              b k
        | Some (False, link, _) -> link
        | None -> "" ]
    | _ -> "" ]
  else ""
;

(* obsolete; should be removed one day *)

value string_of_married_to conf base env p p_auth =
  fun
  [ Vfam fam (_, ispouse) des ->
     let spouse = pget conf base ispouse in
     let auth = p_auth && authorized_age conf base spouse in
     let format = relation_txt conf p.sex fam in
     Printf.sprintf (fcapitale format)
       (fun _ -> if auth then string_of_marriage_text conf base fam else "")
  | _ -> "" ]
;

value string_of_misc_names conf base env p p_auth =
  if p_auth then
    let list = Gutil.person_misc_names base p in
    let list =
      let first_name = p_first_name base p in
      let surname = p_surname base p in
      if first_name <> "?" && surname <> "?" then
        [Name.lower (first_name ^ " " ^ surname) :: list]
      else list
    in
    if list <> [] then
      "<ul>\n" ^
      List.fold_left (fun s n -> s ^ "<li>" ^ n ^ "\n") "" list ^
      "</ul>\n"
    else ""
  else ""
;

value string_of_nobility_title conf base env p p_auth =
  match get_env "nobility_title" env with
  [ Vtitle t when p_auth ->
      string_of_title conf base (transl_nth conf "and" 0) p t
  | _ -> "" ]
;

value obsolete_list = ref [];

value obsolete var new_var =
  if List.mem var obsolete_list.val then ()
  else ifdef UNIX then do {
    Printf.eprintf "*** <W> perso.txt: variable \"%%%s;\" obsolete%s\n"
      var
      (if new_var = "" then "" else "; use rather \"" ^ new_var ^ "%%%s;\"");
    flush stderr;
    obsolete_list.val := [var :: obsolete_list.val]
  }
  else ()
;

value string_of_nobility_titles conf base env p p_auth =
  let () = obsolete "nobility_titles" "nobility_title" in
  if p_auth then string_of_titles conf base True (transl_nth conf "and" 0) p
  else ""
;

value string_of_notes conf base env p p_auth =
  if p_auth then
    let env = [('i', fun () -> Util.default_image_name base p)] in
    string_with_macros conf False env (sou base p.notes)
  else ""
;

value string_of_occupation conf base env p p_auth =
  if p_auth then capitale (sou base p.occupation) else ""
;

value string_of_on_baptism_date conf base env p p_auth =
  if p_auth then
    match Adef.od_of_codate p.baptism with
    [ Some d -> Date.string_of_ondate conf d
    | None -> "" ]
  else ""
;

value string_of_on_birth_date conf base env p p_auth =
  if p_auth then
    match Adef.od_of_codate p.birth with
    [ Some d -> Date.string_of_ondate conf d
    | None -> "" ]
  else ""
;

value string_of_on_burial_date conf base env p p_auth =
  if p_auth then
    match p.burial with
    [ Buried cod ->
        match Adef.od_of_codate cod with
        [ Some d -> Date.string_of_ondate conf d
        | None -> "" ]
    | _ -> "" ]
  else ""
;

value string_of_on_cremation_date conf base env p p_auth =
  if p_auth then
    match p.burial with
    [ Cremated cod ->
        match Adef.od_of_codate cod with
        [ Some d -> Date.string_of_ondate conf d
        | None -> "" ]
    | _ -> "" ]
  else ""
;

value string_of_on_death_date conf base env p p_auth =
  if p_auth then
    match p.death with
    [ Death _ d ->
        let d = Adef.date_of_cdate d in
        Date.string_of_ondate conf d
    | _ -> "" ]
  else ""
;

value string_of_origin_file conf base env =
  if conf.wizard then
    match get_env "fam" env with
    [ Vfam fam _ _ -> sou base fam.origin_file
    | _ -> "" ]
  else ""
;

value string_of_parent_age conf base p a p_auth parent =
  match parents a with
  [ Some ifam ->
      let cpl = coi base ifam in
      let pp = pget conf base (parent cpl) in
      if p_auth && authorized_age conf base pp then
        match (Adef.od_of_codate pp.birth, Adef.od_of_codate p.birth) with
        [ (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
            Date.string_of_age conf (time_gone_by d1 d2)
        | _ -> "" ]
      else ""
  | None -> "" ]
;

value string_of_prefix_no_templ conf base env =
  let henv =
    List.fold_right
      (fun (k, v) henv -> if k = "templ" then henv else [(k, v) :: henv])
      conf.henv []
  in
  let c = conf.command ^ "?" in
  List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ ";") c
    (henv @ conf.senv)
;

value string_of_qualifier conf base env p p_auth =
  match (get_env "qualifier" env, p.qualifiers) with
  [ (Vstring nn, _) -> nn
  | (_, [nn :: _]) -> sou base nn
  | _ -> "" ]
;

value string_of_referer conf base env =
  Wserver.extract_param "referer: " '\n' conf.request
;

value string_of_related_type conf base env =
  match (get_env "c" env, get_env "rel" env) with
  [ (Vind c _ _, Vrel r) ->
      capitale (rchild_type_text conf r.r_type (index_of_sex c.sex))
  | _ -> "" ]
;

value string_of_relation_type conf base env =
  match get_env "rel" env with
  [ Vrel r ->
      match (r.r_fath, r.r_moth) with
      [ (Some ip, None) -> capitale (relation_type_text conf r.r_type 0)
      | (None, Some ip) -> capitale (relation_type_text conf r.r_type 1)
      | (Some ip1, Some ip2) -> capitale (relation_type_text conf r.r_type 2)
      | _ -> "" ]
  | _ -> "" ]
;

value string_of_sosa conf base env a a_auth =
  match get_env "sosa" env with
  [ Vsosa x ->
      match x with
      [ Some (n, p) ->
          let b = Buffer.create 25 in
          do {
            Num.print (fun x -> Buffer.add_string b x)
              (transl conf "(thousand separator)") n;
            Buffer.contents b
          }
      | None -> "" ]
  | _ -> "" ]
;

value string_of_sosa_link conf base env a a_auth =
  match get_env "sosa" env with
  [ Vsosa x ->
      match x with
      [ Some (n, p) ->
          Printf.sprintf "m=RL;i1=%d;i2=%d;b1=1;b2=%s"
            (Adef.int_of_iper a.cle_index) (Adef.int_of_iper p.cle_index)
            (Num.to_string n)
      | None -> "" ]
  | _ -> "" ]
;

value string_of_sosa_ref conf base env a a_auth =
  match get_env "sosa" env with
  [ Vsosa (Some (_, p)) ->
      let p_auth = authorized_age conf base p in
      simple_person_text conf base p p_auth
  | _ -> "" ]
;

value string_of_source conf base env p =
  match get_env "src" env with
  [ Vstring s ->
      let env = [('i', fun () -> Util.default_image_name base p)] in
      string_with_macros conf False env s
  | _ -> "" ]
;

value string_of_source_type conf base env =
  match get_env "src_typ" env with
  [ Vstring s -> s
  | _ -> "" ]
;

value string_of_surname_alias conf base env =
  match get_env "surname_alias" env with
  [ Vstring s -> s
  | _ -> "" ]
;  

value string_of_witness_relation conf base env =
  fun
  [ Vfam _ (ip1, ip2) _ ->
      Printf.sprintf
        (fcapitale (ftransl conf "witness at marriage of %s and %s"))
        (referenced_person_title_text conf base (pget conf base ip1))
        (referenced_person_title_text conf base (pget conf base ip2))
  | _ -> "" ]
;

value eval_int_env var env =
  match get_env var env with
  [ Vint x -> string_of_int x
  | _ -> "" ]
;

value try_eval_gen_variable conf base env (p, a, u, p_auth) efam =
  fun
  [ "alias" -> string_of_alias conf base env
  | "access" -> acces conf base p
  | "age" -> string_of_age conf base env p p_auth
  | "baptism_place" -> string_of_baptism_place conf base env p p_auth
  | "birth_place" -> string_of_birth_place conf base env p p_auth
  | "burial_place" -> string_of_burial_place conf base env p p_auth
  | "border" -> string_of_int conf.border
  | "child_cnt" -> eval_int_env "child_cnt" env
  | "child_name" ->
      let force_surname =
        match parents a with
        [ None -> False
        | Some ifam ->
            p_surname base (pget conf base (father (coi base ifam))) <>
              p_surname base p ]
      in
      if not p_auth && conf.hide_names then "x x"
      else if force_surname then person_text conf base p
      else person_text_without_surname conf base p
  | "comment" -> string_of_comment conf base env p p_auth efam
  | "consanguinity" -> string_of_consanguinity conf base env a p_auth
  | "count" ->
      let () = obsolete "count" "child_cnt" in
      eval_int_env "child_cnt" env
  | "cremation_place" -> string_of_burial_place conf base env p p_auth
  | "dates" ->
      if p_auth then Date.short_dates_text conf base p else ""
  | "death_age" -> string_of_death_age conf base env p p_auth
  | "death_place" -> string_of_death_place conf base env p p_auth
  | "died" -> string_of_died conf base env p p_auth
  | "divorce_date" -> string_of_divorce_date conf base env p p_auth efam
  | "fam_access" ->
      match efam with
      [ Vfam fam _ _ ->
          Printf.sprintf "i=%d;ip=%d" (Adef.int_of_ifam fam.fam_index)
            (Adef.int_of_iper p.cle_index)
      | _ -> "" ]
  | "family_cnt" -> eval_int_env "family_cnt" env
  | "father_age_at_birth" ->
      string_of_parent_age conf base p a p_auth (fun cpl -> (father cpl))
  | "first_name" ->
      if not p_auth && conf.hide_names then "x" else p_first_name base p
  | "first_name_alias" -> string_of_first_name_alias conf base env
  | "first_name_key" ->
      if conf.hide_names && not p_auth then ""
      else code_varenv (Name.lower (p_first_name base p))
  | "image_html_url" -> string_of_image_url conf base env p p_auth True
  | "image_txt" -> default_image_name base p
  | "image_size" -> string_of_image_size conf base env p p_auth
  | "image_url" -> string_of_image_url conf base env p p_auth False
  | "ind_access" -> "i=" ^ string_of_int (Adef.int_of_iper p.cle_index)
  | "key" ->
      if not p_auth && conf.hide_names then "x"
      else Util.default_image_name base p
  | "length" ->
      let () = obsolete "length" "nb_children" in
      match get_env "fam" env with
      [ Vfam _ _ des -> string_of_int (Array.length des.children)
      | _ -> "" ]
  | "marriage_place" ->
      if p_auth then
        match get_env "fam" env with
        [ Vfam fam _ _ -> sou base fam.marriage_place
        | _ -> "" ]
      else ""
  | "married_to" ->
      let () = obsolete "married_to" "" in
      string_of_married_to conf base env p p_auth efam
  | "misc_names" -> string_of_misc_names conf base env p p_auth
  | "mother_age_at_birth" ->
      string_of_parent_age conf base p a p_auth (fun cpl -> (mother cpl))
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
  | "nobility_title" -> string_of_nobility_title conf base env p p_auth
  | "nobility_titles" -> string_of_nobility_titles conf base env p p_auth
  | "notes" -> string_of_notes conf base env p p_auth
  | "occupation" -> string_of_occupation conf base env p p_auth
  | "on_baptism_date" -> string_of_on_baptism_date conf base env p p_auth
  | "on_birth_date" -> string_of_on_birth_date conf base env p p_auth
  | "on_burial_date" -> string_of_on_burial_date conf base env p p_auth
  | "on_cremation_date" -> string_of_on_cremation_date conf base env p p_auth
  | "on_death_date" -> string_of_on_death_date conf base env p p_auth
  | "on_marriage_date" ->
      if p_auth then
        match get_env "fam" env with
        [ Vfam fam _ _ ->
            match Adef.od_of_codate fam.marriage with
            [ Some s -> Date.string_of_ondate conf s
            | None -> "" ] 
        | _ -> "" ]
      else ""
  | "origin_file" -> string_of_origin_file conf base env
  | "prefix_no_templ" -> string_of_prefix_no_templ conf base env
  | "public_name" ->  sou base p.public_name
  | "qualifier" -> string_of_qualifier conf base env p p_auth
  | "referer" -> string_of_referer conf base env
  | "relation_type" -> string_of_relation_type conf base env
  | "related_type" -> string_of_related_type conf base env
  | "sosa" -> string_of_sosa conf base env p p_auth
  | "sosa_link" -> string_of_sosa_link conf base env p p_auth
  | "sosa_ref" -> string_of_sosa_ref conf base env p p_auth
  | "source" -> string_of_source conf base env p
  | "source_type" -> string_of_source_type conf base env
  | "surname" ->
      if not p_auth && conf.hide_names then "x" else p_surname base p
  | "surname_alias" -> string_of_surname_alias conf base env
  | "surname_key" ->
      if conf.hide_names && not p_auth then ""
      else code_varenv (Name.lower (p_surname base p))
  | "title" -> person_title conf base p
  | "witness_relation" -> string_of_witness_relation conf base env efam
  | s ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> quote_escaped vv
        | _ -> "" ]
      else raise Not_found ]
;

value print_simple_person_text conf base (p, _, _, p_auth) =
  Wserver.wprint "%s" (simple_person_text conf base p p_auth)
;

value print_variable conf base env sl =
  match eval_variable conf base env sl with
  [ VVsome (env, ep, efam, "") -> print_simple_person_text conf base ep
  | VVsome (env, ep, efam, s) ->
      try
        Wserver.wprint "%s" (try_eval_gen_variable conf base env ep efam s)
      with
      [ Not_found -> Templ.print_variable conf base s ]
  | VVcvar s ->
      Wserver.wprint "%s" (eval_base_env_variable conf s)
  | VVnone ->
      do {
        list_iter_first
          (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
          sl;
        Wserver.wprint "???"
      } ]
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
  | "are_engaged" ->
      match efam with
      [ Vfam fam _ _ -> fam.relation = Engaged
      | _ -> False ]
  | "are_married" ->
      match efam with
      [ Vfam fam _ _ -> fam.relation = Married
      | _ -> False ]
  | "are_not_married" ->
      match efam with
      [ Vfam fam _ _ -> fam.relation = NotMarried
      | _ -> False ]
  | "are_separated" ->
      match efam with
      [ Vfam fam cpl _ -> fam.divorce = Separated
      | _ -> False ]
  | "is_no_sexes_check" ->
      match efam with
      [ Vfam fam _ _ -> fam.relation = NoSexesCheck
      | _ -> False ]
  | "is_no_mention" ->
      match efam with
      [ Vfam fam _ _ -> fam.relation = NoMention
      | _ -> False ]
  | "birthday" ->
      if p_auth then
        match Adef.od_of_codate p.birth with
        [ Some (Dgreg d _) ->
            if d.prec = Sure && p.death = NotDead then
              d.day = conf.today.day && d.month = conf.today.month &&
              d.year < conf.today.year ||
              not (leap_year conf.today.year) && d.day = 29 && d.month = 2 &&
              conf.today.day = 1 && conf.today.month = 3
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
           Some (Dgreg ({prec = Sure | About | Maybe} as d2) _), approx)
          when d1 <> d2 ->
            let a = time_gone_by d1 d2 in
            a.year > 0 ||
            a.year = 0 && (a.month > 0 || a.month = 0 && a.day > 0)
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
               let des = doi base ifam in Array.length des.children > 0)
            (Array.to_list u.family) ]
  | "has_comment" ->
      match efam with
      [ Vfam fam _ _ -> p_auth && sou base fam.comment <> ""
      | _ -> False ]
  | "has_consanguinity" ->
      p_auth && consang a != Adef.fix (-1) && consang a != Adef.fix 0
  | "has_cremation_place" -> p_auth && sou base p.burial_place <> ""
  | "has_death_date" ->
      match p.death with
      [ Death _ _ -> p_auth
      | _ -> False ]
  | "has_death_place" -> p_auth && sou base p.death_place <> ""
  | "has_families" -> Array.length u.family > 0
  | "has_first_names_aliases" -> p.first_names_aliases <> []
  | "has_image" -> Util.has_image conf base p
  | "has_nephews_or_nieces" -> has_nephews_or_nieces conf base p
  | "has_nobility_titles" -> p_auth && p.titles <> []
  | "has_notes" -> p_auth && sou base p.notes <> ""
  | "has_occupation" -> p_auth && sou base p.occupation <> ""
  | "has_parents" -> parents a <> None
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
  | "has_relations" ->
      if p_auth && conf.use_restrict then
        let related =
          List.fold_left
            (fun l ip ->
               let rp = pget conf base ip in
               if is_hidden rp then l else [ip :: l])
          [] p.related
        in
        (p.rparents <> [] || related <> [])
      else p_auth && (p.rparents <> [] || p.related <> [])
  | "has_siblings" ->
      match parents a with
      [ Some ifam -> Array.length (doi base ifam).children > 1
      | None -> False ]
  | "has_sosa" ->
      match get_env "sosa" env with
      [ Vsosa x -> x <> None
      | _ -> False ]
  | "has_sources" ->
      if conf.hide_names && not p_auth then False
      else if sou base p.psources <> "" then True
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
  | "is_restricted" -> is_hidden p
  | "is_self" -> get_env "pos" env = Vstring "self"
  | "wizard" -> conf.wizard
  | "friend" -> conf.friend
  | v ->
      let v = extract_var "file_exists_" v in
      if v <> "" then
        let v = code_varenv v in
        let s = Srcfile.source_file_name conf v in
        Sys.file_exists s
      else do { Wserver.wprint ">%%%s???" v; False } ]
;

value eval_bool_variable conf base env sl =
  match eval_variable conf base env sl with
  [ VVsome (env, ep, efam, "") ->
      do {
        list_iter_first
          (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
          sl;
        Wserver.wprint "???";
        False
      }
  | VVsome (env, ep, efam, s) ->
      eval_simple_bool_variable conf base env ep efam s
  | VVcvar s -> do { Wserver.wprint ">%%%s???" s; False }
  | VVnone -> False ]
;

value print_transl conf base env upp s c =
  let r =
    match c with
    [ "n" | "s" | "w" ->
        let n =
          match c with
          [ "n" ->
              match get_env "p" env with
              [ Vind p _ _ -> 1 - index_of_sex p.sex
              | _ -> 2 ]
          | "s" ->
              match get_env "child" env with
              [ Vind p _ _ -> index_of_sex p.sex
              | _ ->
                  match get_env "p" env with
                  [ Vind p _ _ -> index_of_sex p.sex
                  | _ -> 2 ] ]
          | "w" ->
              match get_env "fam" env with
              [ Vfam fam _ _ -> if Array.length fam.witnesses = 1 then 0 else 1
              | _ -> 0 ]
          | _ -> assert False ]
        in
        let r = Util.transl_nth conf s n in
        if upp then capitale r else r
    | _ ->
        Templ.eval_transl conf upp s c ]
  in
  Wserver.wprint "%s" r
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
        | _ -> do { Wserver.wprint "op %s???" op; False } ]
    | Enot e -> not (bool_eval e)
    | Evar s sl -> eval_bool_variable conf base env [s :: sl]
    | Estr s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Eint s -> do { Wserver.wprint "\"%s\"???" s; False }
    | Etransl _ s _ -> do { Wserver.wprint "[%s]???" s; False } ]
  and string_eval =
    fun
    [ Estr s -> s
    | Eint s -> s
    | Evar s sl ->
        try
          match eval_variable conf base env [s :: sl] with
          [ VVsome (env, ((p, _, _, p_auth) as ep), efam, s) ->
              if s <> "" then try_eval_gen_variable conf base env ep efam s
              else simple_person_text conf base p p_auth
          | VVcvar s -> eval_base_env_variable conf s
          | _ -> raise Not_found ]
        with
        [ Not_found -> do { Wserver.wprint ">%%%s???" s; "" } ]
    | Etransl upp s c -> Templ.eval_transl conf upp s c
    | x -> do { Wserver.wprint "val???"; "" } ]
  in
  bool_eval
;

value eval_expr conf base env p =
  fun
  [ Estr s -> s
  | Evar s [] ->
      try try_eval_gen_variable conf base env p Vnone s with
      [ Not_found -> ">" ^ s ^ "???" ]
  | Etransl upp s c -> Templ.eval_transl conf upp s c
  | _ -> ">parse_error" ]
;

value rec print_ast conf base env =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n -> print_transl conf base env upp s n
  | Avar s sl -> print_variable conf base env [s :: sl]
  | Awid_hei s -> print_wid_hei conf base env s
  | Aif e alt ale -> print_if conf base env e alt ale
  | Aforeach s sl al -> print_foreach conf base env s sl al
  | Adefine f xl al alk -> print_define conf base env f xl al alk
  | Aapply f el -> print_apply conf base env f el ]
and print_define conf base env f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env]) alk
and print_apply conf base env f el =
  match get_env f env with
  [ Vfun xl al ->
      let ep =
        match (get_env "p" env, get_env "p_auth" env) with
        [ (Vind p a u, Vbool p_auth) -> (p, a, u, p_auth)
        | _ -> assert False ]
      in
      let vl = List.map (eval_expr conf base env ep) el in
      List.iter
        (fun a ->
           let a =
             loop a xl vl where rec loop a xl vl =
               match (xl, vl) with
               [ ([x :: xl], [v :: vl]) ->
                   loop (Templ.subst (Templ.subst_text x v) a) xl vl
               | ([], []) -> a
               | _ -> Atext "parse_error" ]
           in
           print_ast conf base env a)
        al
  | _ -> Wserver.wprint ">%%%s???" f ]
and print_if conf base env e alt ale =
  let al = if eval_bool_value conf base env e then alt else ale in
  List.iter (print_ast conf base env) al
and print_foreach conf base env s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env sl with
  [ VVsome (env, ep, efam, "") ->
      print_simple_foreach conf base env al ep efam s
  | VVsome (env, ep, efam, _) ->
      do {
        Wserver.wprint "foreach ";
        List.iter (fun s -> Wserver.wprint "%s." s) sl;
        Wserver.wprint "%s???" s
      }
 | VVcvar s -> Wserver.wprint ">%%%s???" s
 | VVnone -> () ]
and print_simple_foreach conf base env al ep efam =
  fun
  [ "alias" -> print_foreach_alias conf base env al ep
  | "child" -> print_foreach_child conf base env al efam
  | "family" -> print_foreach_family conf base env al ep
  | "first_name_alias" -> print_foreach_first_name_alias conf base env al ep
  | "nobility_title" -> print_foreach_nobility_title conf base env al ep
  | "parent" -> print_foreach_parent conf base env al ep
  | "qualifier" -> print_foreach_qualifier conf base env al ep
  | "related" -> print_foreach_related conf base env al ep
  | "relation" -> print_foreach_relation conf base env al ep
  | "source" -> print_foreach_source conf base env al ep
  | "surname_alias" -> print_foreach_surname_alias conf base env al ep
  | "witness" -> print_foreach_witness conf base env al efam
  | "witness_relation" -> print_foreach_witness_relation conf base env al ep
  | s -> Wserver.wprint "foreach %s???" s ]
and print_foreach_alias conf base env al (p, _, _, p_auth) =
  List.iter
    (fun a ->
       let env = [("alias", Vstring (sou base a)) :: env] in
       List.iter (print_ast conf base env) al)
    p.aliases
and print_foreach_child conf base env al =
  fun
  [ Vfam _ _ des ->
      let auth =
        List.for_all (fun ip -> authorized_age conf base (pget conf base ip))
          (Array.to_list des.children)
      in
      let env = [("auth", Vbool auth) :: env] in
      let n =
        let p =
          match get_env "p" env with
          [ Vind p _ _ -> p
          | _ -> assert False ]
        in
        let rec loop i =
          if i = Array.length des.children then -2
          else if des.children.(i) = p.cle_index then i
          else loop (i + 1)
        in
        loop 0
      in
      Array.iteri
        (fun i ip ->
           let p = pget conf base ip in
           let a = aget conf base ip in
           let u = uget conf base ip in
           let env = [("#loop", Vint 0) :: env] in
           let env = [("child", Vind p a u) :: env] in
           let env = [("child_cnt", Vint (i + 1)) :: env] in
           let env =
             if i = n - 1 && not (is_hidden p) then
               [("pos", Vstring "prev") :: env]
             else if i = n then [("pos", Vstring "self") :: env]
             else if i = n + 1 && not (is_hidden p) then
               [("pos", Vstring "next") :: env]
             else env
           in
           List.iter (print_ast conf base env) al)
        des.children
  | _ -> () ]
and print_foreach_family conf base env al (p, _, u, _) =
  Array.iteri
    (fun i ifam ->
       let fam = foi base ifam in
       let cpl = coi base ifam in
       let des = doi base ifam in
       let cpl = (p.cle_index, spouse p.cle_index cpl) in
       let env = [("#loop", Vint 0) :: env] in
       let env = [("fam", Vfam fam cpl des) :: env] in
       let env = [("family_cnt", Vint (i + 1)) :: env] in
       List.iter (print_ast conf base env) al)
    u.family
and print_foreach_first_name_alias conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun s ->
         let env = [("first_name_alias", Vstring (sou base s)) :: env] in
         List.iter (print_ast conf base env) al)
      p.first_names_aliases
  else ()
and print_foreach_nobility_title conf base env al (p, _, _, p_auth) =
  if p_auth then
    let titles = nobility_titles_list conf p in
    list_iter_first
      (fun first x ->
         let env = [("nobility_title", Vtitle x) :: env] in
         let env = [("first", Vbool first) :: env] in
         List.iter (print_ast conf base env) al)
      titles
  else ()
and print_foreach_parent conf base env al (_, a, _, _) =
  match parents a with
  [ Some ifam ->
      let cpl = coi base ifam in
      Array.iter
        (fun iper ->
	   let p = pget conf base iper in
	   let a = aget conf base iper in
	   let u = uget conf base iper in
	   let env = [("parent", Vind p a u) :: env] in
           List.iter (print_ast conf base env) al)
        (parent_array cpl)
  | None -> () ]
and print_foreach_qualifier conf base env al (p, _, _, _) =
  list_iter_first
    (fun first nn ->
       let env = [("qualifier", Vstring (sou base nn)) :: env] in
       let env = [("first", Vbool first) :: env] in
       List.iter (print_ast conf base env) al)
    p.qualifiers
and print_foreach_relation conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun r ->
         let env = [("rel", Vrel r) :: env] in
         List.iter (print_ast conf base env) al)
      p.rparents
  else ()
and print_foreach_related conf base env al (p, _, _, p_auth) =
  if p_auth then
    let list =
      List.fold_left
        (fun list ic ->
           let c = pget conf base ic in
           loop c.rparents where rec loop =
             fun
             [ [r :: rl] ->
                 match r.r_fath with
                 [ Some ip when ip = p.cle_index -> [(c, r) :: list]
                 | _ ->
                     match r.r_moth with
                     [ Some ip when ip = p.cle_index -> [(c, r) :: list]
                     | _ -> loop rl ] ]
             | [] -> list ])
        [] p.related
    in
    let list =
      List.sort
        (fun (c1, _) (c2, _) ->
           let d1 =
             match Adef.od_of_codate c1.baptism with
             [ None -> Adef.od_of_codate c1.birth
             | x -> x ]
           in
           let d2 =
             match Adef.od_of_codate c2.baptism with
             [ None -> Adef.od_of_codate c2.birth
             | x -> x ]
           in
           match (d1, d2) with
           [ (Some d1, Some d2) ->
               if strictly_before d1 d2 then -1 else 1
           | _ -> -1 ])
      (List.rev list)
    in
    List.iter
      (fun (c, r) ->
         let a = aget conf base c.cle_index in
         let u = uget conf base c.cle_index in
         let env = [("c", Vind c a u); ("rel", Vrel r) :: env] in
         List.iter (print_ast conf base env) al)
      list
  else ()
and print_foreach_source conf base env al (p, _, u, p_auth) =
  let rec insert_loop typ src =
    fun
    [ [(typ1, src1) :: srcl] ->
        if src = src1 then [(typ1 ^ ", " ^ typ, src1) :: srcl]
        else [(typ1, src1) :: insert_loop typ src srcl]
    | [] -> [(typ, src)] ]
  in
  let insert typ src srcl = insert_loop (nominative typ) src srcl in
  let srcl = [] in
  let srcl =
    if not conf.hide_names || p_auth then
      insert (transl_nth conf "person/persons" 0) p.psources srcl
    else srcl
  in
  let srcl =
    if p_auth then
      let srcl = insert (transl_nth conf "birth" 0) p.birth_src srcl in
      let srcl = insert (transl_nth conf "baptism" 0) p.baptism_src srcl in
      let srcl = insert (transl_nth conf "death" 0) p.death_src srcl in
      let srcl = insert (transl_nth conf "burial" 0) p.burial_src srcl in srcl
    else srcl
  in
  let (srcl, _) =
    Array.fold_left
      (fun (srcl, i) ifam ->
         let fam = foi base ifam in
         let lab =
           if Array.length u.family == 1 then "" else " " ^ string_of_int i
         in
         let srcl =
           if p_auth then
             let src_typ = transl_nth conf "marriage/marriages" 0 in
             insert (src_typ ^ lab) fam.marriage_src srcl
           else srcl
         in
         let src_typ = transl_nth conf "family/families" 0 in
         (insert (src_typ ^ lab) fam.fsources srcl, i + 1))
      (srcl, 1) u.family
  in
  let print_src (src_typ, src) =
    let s = sou base src in
    if s = "" then ()
    else
      let env = [("src_typ", Vstring src_typ); ("src", Vstring s) :: env] in
      List.iter (print_ast conf base env) al
  in
  List.iter print_src srcl
and print_foreach_surname_alias conf base env al (p, _, _, _) =
  List.iter
    (fun s ->
       let env = [("surname_alias", Vstring (sou base s)) :: env] in
       List.iter (print_ast conf base env) al)
    p.surnames_aliases
and print_foreach_witness conf base env al =
  fun
  [ Vfam fam _ _ ->
      list_iter_first
        (fun first ip ->
           let p = pget conf base ip in
           let a = aget conf base ip in
           let u = uget conf base ip in
           let env = [("witness", Vind p a u) :: env] in
           let env = [("first", Vbool first) :: env] in
           List.iter (print_ast conf base env) al)
        (Array.to_list fam.witnesses)
  | _ -> () ]
and print_foreach_witness_relation conf base env al (p, _, _, _) =
  let list =
    let list = ref [] in
    do {
      make_list p.related where rec make_list =
        fun
        [ [ic :: icl] ->
            do {
              let c = pget conf base ic in
              if c.sex = Male then
                Array.iter
                  (fun ifam ->
                     let fam = foi base ifam in
                     if array_memq p.cle_index fam.witnesses then
                       list.val := [(ifam, fam) :: list.val]
                     else ())
                  (uget conf base ic).family
              else ();
              make_list icl
            }
        | [] -> () ];
      list.val
    }
  in
  let list =
    List.sort
      (fun (_, fam1) (_, fam2) ->
	 match
	   (Adef.od_of_codate fam1.marriage, Adef.od_of_codate fam2.marriage)
	 with
	 [ (Some d1, Some d2) ->
	     if strictly_before d1 d2 then -1
	     else if strictly_before d2 d1 then 1
	     else 0
	 | _ -> 0 ])
      list
  in
  List.iter
    (fun (ifam, fam) ->
       let cpl = coi base ifam in
       let des = doi base ifam in
       let cpl = ((father cpl), (mother cpl)) in
       let env = [("fam", Vfam fam cpl des) :: env] in
       List.iter (print_ast conf base env) al)
    list
;

value interp_templ conf base p astl =
  let a = aget conf base p.cle_index in
  let u = uget conf base p.cle_index in
  let env =
    let env = [] in
    let env =
      let v = find_sosa conf base p in [("sosa", Vsosa v) :: env]
    in
    let env =
      let v =
        image_and_size conf base p (limited_image_size max_im_wid max_im_wid)
      in
      [("image", Vimage v) :: env]
    in
    let env = [("p_auth", Vbool (authorized_age conf base p)) :: env] in
    let env = [("p", Vind p a u) :: env] in env
  in
  List.iter (print_ast conf base env) astl
;

(* Main *)

value print_ok conf base p =
  let astl = Templ.input conf "perso" in
  do {
    html conf;
    nl ();
    interp_templ conf base p astl
  }
;

value print conf base p =
  let passwd =
    if conf.wizard || conf.friend then None
    else
      let src =
        match parents (aget conf base p.cle_index) with
        [ Some ifam -> sou base (foi base ifam).origin_file
        | None -> "" ]
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.base_env) with
      [ Not_found -> None ]
  in
  match passwd with
  [ Some (src, passwd) when passwd <> conf.passwd ->
      Util.unauthorized conf src
  | _ -> print_ok conf base p ]
;
