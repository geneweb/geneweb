(* camlp4r *)
(* $Id: relmenu.ml,v 4.6 2005-05-12 13:42:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Gutil;
open Config;
open Util;
open TemplAst;

type env = 
  [ Vind of person
  | Vfam of Adef.ifam
  | Vrel of relation and option person
  | Vfun of list string and list ast
  | Vnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  Printf.sprintf ">%s<p>\n" ("Relmenu." ^ func ^ ": not impl " ^ desc)
;

value rec eval_var conf base env ep loc sl =
  try VVbool (eval_bool_var conf base env ep loc sl) with
  [ Not_found -> VVstring (eval_str_var conf base env ep loc sl) ]
and eval_bool_var conf base env (p, p_auth_name) loc =
  fun
  [ ["access_by_key"] ->
      Util.accessible_by_key conf base p (p_first_name base p)
        (p_surname base p)
  | ["has_relation_her"] ->
      match get_env "relation" env with
      [ Vrel {r_moth = Some _} None -> True
      | _ -> False ]
  | ["has_relation_him"] ->
      match get_env "relation" env with
      [ Vrel {r_fath = Some _} None -> True
      | _ -> False ]
  | ["browsing_with_sosa_ref"] -> Util.find_sosa_ref conf base <> None
  | _ -> raise Not_found ]
and eval_str_var conf base env (p, p_auth_name) loc =
  fun
  [ ["alias"] ->
      match p.aliases with
      [ [nn :: _] -> if not p_auth_name then "" else sou base nn
      | [] -> "" ]
  | ["first_name"] -> if not p_auth_name then "x" else p_first_name base p
  | ["first_name_key_val"] ->
      if not p_auth_name then "" else Name.lower (p_first_name base p)
  | ["index"] -> string_of_int (Adef.int_of_iper p.cle_index)
  | ["occ"] -> if p_auth_name then string_of_int p.occ else ""
  | ["public_name"] -> if not p_auth_name then "" else sou base p.public_name
  | ["qualifier"] ->
      match p.qualifiers with
      [ [nn :: _] -> if not p_auth_name then "" else sou base nn
      | [] -> "" ]
  | ["related"] ->
      match get_env "related" env with
      [ Vrel _ (Some p) -> simple_person_text conf base p p_auth_name
      | _ -> "" ]
  | ["related"; "index"] ->
      match get_env "related" env with
      [ Vrel _ (Some p) -> string_of_int (Adef.int_of_iper p.cle_index)
      | _ -> "" ]
  | ["related"; "nobility_title"] ->
      match get_env "related" env with
      [ Vrel _ (Some p) ->
          match Util.main_title base p with
          [ Some t when p_auth_name ->
              let id = sou base t.t_ident in
              let pl = sou base t.t_place in
              if pl = "" then id else id ^ " " ^ pl
          | _ -> "" ]
      | _ -> "" ]
  | ["related"; "type"] ->
      match get_env "related" env with
      [ Vrel r (Some c) -> rchild_type_text conf r.r_type (index_of_sex c.sex)
      | _ -> "" ]
  | ["relation_her"] ->
      match get_env "relation" env with
      [ Vrel {r_moth = Some ip} None ->
           simple_person_text conf base (poi base ip) p_auth_name
      | _ -> "" ]
  | ["relation_him"] ->
      match get_env "relation" env with
      [ Vrel {r_fath = Some ip} None ->
          simple_person_text conf base (poi base ip) p_auth_name
      | _ -> "" ]
  | ["relation_her"; "nobility_title"] ->
      match get_env "relation" env with
      [ Vrel {r_moth = Some ip} None ->
          match Util.main_title base (poi base ip) with
          [ Some t when p_auth_name ->
              let id = sou base t.t_ident in
              let pl = sou base t.t_place in
              if pl = "" then id else id ^ " " ^ pl
          | _ -> "" ]
      | _ -> "" ]
  | ["relation_him"; "nobility_title"] ->
      match get_env "relation" env with
      [ Vrel {r_fath = Some ip} None ->
          match Util.main_title base (poi base ip) with
          [ Some t when p_auth_name ->
              let id = sou base t.t_ident in
              let pl = sou base t.t_place in
              if pl = "" then id else id ^ " " ^ pl
          | _ -> "" ]
      | _ -> "" ]
  | ["relation_her"; "index"] ->
      match get_env "relation" env with
      [ Vrel {r_moth = Some ip} None -> string_of_int (Adef.int_of_iper ip)
      | _ -> "" ]
  | ["relation_him"; "index"] ->
      match get_env "relation" env with
      [ Vrel {r_fath = Some ip} None -> string_of_int (Adef.int_of_iper ip)
      | _ -> "" ]
  | ["relation_her"; "type"] ->
      match get_env "relation" env with
      [ Vrel {r_type = rt} None -> relation_type_text conf rt 1
      | _ -> "" ]
  | ["relation_him"; "type"] ->
      match get_env "relation" env with
      [ Vrel {r_type = rt} None -> relation_type_text conf rt 0
      | _ -> "" ]
  | ["sosa_ref"] ->
      match Util.find_sosa_ref conf base with
      [ Some p -> person_title_text conf base p
      | None -> "" ]
  | ["sosa_ref"; "access"] ->
      match Util.find_sosa_ref conf base with
      [ Some p -> acces conf base p
      | None -> "" ]
  | ["spouse"] ->
      match get_env "fam" env with
      [ Vfam ifam ->
          let p = poi base (spouse p.cle_index (coi base ifam)) in
          simple_person_text conf base p p_auth_name
      | _ -> raise Not_found ]
  | ["spouse"; "index"] ->
      match get_env "fam" env with
      [ Vfam ifam ->
          let ip = spouse p.cle_index (coi base ifam) in
          string_of_int (Adef.int_of_iper ip)
      | _ -> raise Not_found ]
  | ["spouse"; "nobility_title"] ->
      match get_env "fam" env with
      [ Vfam ifam ->
          let ip = spouse p.cle_index (coi base ifam) in
          match Util.main_title base (poi base ip) with
          [ Some t when p_auth_name ->
              let id = sou base t.t_ident in
              let pl = sou base t.t_place in
              if pl = "" then id else id ^ " " ^ pl
          | _ -> "" ]
      | _ -> raise Not_found ]
  | ["surname"] -> if not p_auth_name then "x" else p_surname base p
  | ["surname_key_val"] ->
      if not p_auth_name then "" else Name.lower (p_surname base p)
  | ["witness"] ->
      match get_env "witness" env with
      [ Vind p -> simple_person_text conf base p p_auth_name
      | _ -> raise Not_found ]
  | ["witness"; "index"] ->
      match get_env "witness" env with
      [ Vind p -> string_of_int (Adef.int_of_iper p.cle_index)
      | _ -> raise Not_found ]
  | _ -> raise Not_found ]
and simple_person_text conf base p p_auth =
  if p_auth then
    match main_title base p with
    [ Some t -> titled_person_text conf base p t
    | None -> person_text conf base p ]
  else if conf.hide_names then "x x"
  else person_text conf base p
;

value rec eval_ast conf base env ep =
  fun
  [ Atext s -> s
  | Avar loc s sl ->
      Templ.eval_string_var conf (eval_var conf base env ep loc) s sl
  | Atransl upp s c -> Templ.eval_transl conf upp s c
  | AapplyWithAst f all -> eval_apply conf base env ep f all
  | x -> not_impl "eval_ast" x ]
and eval_apply conf base env ep f all =
  match get_env f env with
  [ Vfun xl al ->
      let eval_ast = eval_ast conf base env ep in
      Templ.eval_apply f eval_ast xl al all
  | _ -> Printf.sprintf "%%apply;%s?" f ]
;

value rec print_ast conf base env ep =
  fun
  [ Avar loc s sl ->
      Templ.print_var conf base (eval_var conf base env ep loc) s sl
  | Aif e alt ale -> print_if conf base env ep e alt ale
  | Awid_hei s ->
      match image_size (image_file_name s) with
      [ Some (wid, hei) -> Wserver.wprint " width=\"%d\" height=\"%d\"" wid hei
      | None -> () ]
  | Aforeach v al -> print_foreach conf base env ep v al
  | Adefine f xl al alk -> print_define conf base env ep f xl al alk
  | Aapply f el -> print_apply conf base env ep f el
  | x -> Wserver.wprint "%s" (eval_ast conf base env ep x) ]
and print_define conf base env ep f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] ep) alk
and print_apply conf base env p f el =
  match get_env f env with
  [ Vfun xl al ->
      let eval_var = eval_var conf base env p in
      let print_ast = print_ast conf base env p in
      Templ.print_apply conf f print_ast eval_var xl al el
  | _ -> Wserver.wprint " %%%s?" f ]
and print_if conf base env p e alt ale =
  let eval_var = eval_var conf base env p in
  let al = if Templ.eval_bool_expr conf eval_var e then alt else ale in
  List.iter (print_ast conf base env p) al
and print_foreach conf base env ep (loc, s, sl) al =
  try
    match [s :: sl] with
    [ ["family"] -> print_foreach_family conf base env ep al
    | ["related"] -> print_foreach_related conf base env ep al
    | ["relation"] -> print_foreach_relation conf base env ep al
    | ["witness"] -> print_foreach_witness conf base env ep al
    | _ -> raise Not_found ]
  with
  [ Not_found ->
      do {
        Wserver.wprint ">%%foreach;%s" s;
        List.iter (fun s -> Wserver.wprint ".%s" s) sl;
        Wserver.wprint "?";
       } ]
and print_foreach_family conf base env ((p, _) as ep) al =
  let u = uoi base p.cle_index in
  Array.iter
    (fun ifam ->
       let env = [("fam", Vfam ifam) :: env] in
       List.iter (print_ast conf base env ep) al)
    u.family
and print_foreach_related conf base env ((p, _) as ep) al =
  List.iter
    (fun ip ->
       let c = pget conf base ip in
       if is_hidden c then ()
       else
         List.iter
           (fun r ->
              List.iter
                (fun
                 [ Some ip1 when p.cle_index == ip1 ->
                     let env = [("related", Vrel r (Some c)) :: env] in
                     List.iter (print_ast conf base env ep) al
                 | _ -> () ])
                [r.r_fath; r.r_moth])
           c.rparents)
    p.related
and print_foreach_relation conf base env ((p, _) as ep) al =
  List.iter
    (fun r ->
       let env = [("relation", Vrel r None) :: env] in
       List.iter (print_ast conf base env ep) al)
    p.rparents
and print_foreach_witness conf base env ((p, _) as ep) al =
  match get_env "fam" env with
  [ Vfam ifam ->
      let fam = foi base ifam in
      Array.iter
        (fun ip ->
           let w = pget conf base ip in
           if is_hidden w then ()
           else
             let env = [("witness", Vind w) :: env] in
             List.iter (print_ast conf base env ep) al
        )
        fam.witnesses
  | _ -> raise Not_found ]
;

value print conf base p =
  let astl = Templ.input conf "relmenu" in
  let p_auth_name = authorized_age conf base p || not conf.hide_names in
  let ep = (p, p_auth_name) in
  do {
    Util.html1 conf;
    List.iter (print_ast conf base [] ep) astl;
  }
;

value print conf =
  (if p_getenv conf.env "new" = Some "on" then Perso.interp_templ "relmenu"
   else print) conf
;
